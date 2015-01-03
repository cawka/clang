//===- PrintFunctionNames.cpp ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Example clang plugin which simply prints the names of all the top-level decls
// in the input file.
//
//===----------------------------------------------------------------------===//

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/raw_ostream.h"

#include "clang/AST/RecursiveASTVisitor.h"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/CommentVisitor.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclLookups.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeVisitor.h"
#include "clang/Basic/Module.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"
using namespace clang;
using namespace clang::comments;


namespace {

  struct TerminalColor {
    raw_ostream::Colors Color;
    bool Bold;
  };

  // Red           - CastColor
  // Green         - TypeColor
  // Bold Green    - DeclKindNameColor, UndeserializedColor
  // Yellow        - AddressColor, LocationColor
  // Blue          - CommentColor, NullColor, IndentColor
  // Bold Blue     - AttrColor
  // Bold Magenta  - StmtColor
  // Cyan          - ValueKindColor, ObjectKindColor
  // Bold Cyan     - ValueColor, DeclNameColor

  // Decl kind names (VarDecl, FunctionDecl, etc)
  static const TerminalColor DeclKindNameColor = { raw_ostream::GREEN, true };
  // Attr names (CleanupAttr, GuardedByAttr, etc)
  static const TerminalColor AttrColor = { raw_ostream::BLUE, true };
  // Statement names (DeclStmt, ImplicitCastExpr, etc)
  static const TerminalColor StmtColor = { raw_ostream::MAGENTA, true };

  // Type names (int, float, etc, plus user defined types)
  static const TerminalColor TypeColor = { raw_ostream::GREEN, false };

  // Null statements
  static const TerminalColor NullColor = { raw_ostream::BLUE, false };

  // Undeserialized entities
  static const TerminalColor UndeserializedColor = { raw_ostream::GREEN, true };

  // Decl names
  static const TerminalColor DeclNameColor = { raw_ostream::CYAN, true };

  // Indents ( `, -. | )
  static const TerminalColor IndentColor = { raw_ostream::BLUE, false };

  class ASTDumper
      : public ConstDeclVisitor<ASTDumper>, public ConstStmtVisitor<ASTDumper>,
        public ConstCommentVisitor<ASTDumper>,
        public TypeVisitor<ASTDumper> {
    raw_ostream &OS;

    /// Pending[i] is an action to dump an entity at level i.
    llvm::SmallVector<std::function<void(bool isLastChild)>, 32> Pending;

    /// Indicates whether we're at the top level.
    bool TopLevel;

    /// Indicates if we're handling the first child after entering a new depth.
    bool FirstChild;

    /// Prefix for currently-being-dumped entity.
    std::string Prefix;

    /// The \c FullComment parent of the comment being dumped.
    const FullComment *FC;

    bool ShowColors;

    /// Dump a child of the current node.
    template<typename Fn> void dumpChild(Fn doDumpChild) {
      // If we're at the top level, there's nothing interesting to do; just
      // run the dumper.
      if (TopLevel) {
        TopLevel = false;
        doDumpChild();
        while (!Pending.empty()) {
          Pending.back()(true);
          Pending.pop_back();
        }
        Prefix.clear();
        OS << "\n";
        TopLevel = true;
        return;
      }

      const FullComment *OrigFC = FC;
      auto dumpWithIndent = [this, doDumpChild, OrigFC](bool isLastChild) {
        // Print out the appropriate tree structure and work out the prefix for
        // children of this node. For instance:
        //
        //   A        Prefix = ""
        //   |-B      Prefix = "| "
        //   | `-C    Prefix = "|   "
        //   `-D      Prefix = "  "
        //     |-E    Prefix = "  | "
        //     `-F    Prefix = "    "
        //   G        Prefix = ""
        //
        // Note that the first level gets no prefix.
        {
          OS << '\n';
          ColorScope Color(*this, IndentColor);
          OS << Prefix << (isLastChild ? '`' : '|') << '-';
          this->Prefix.push_back(isLastChild ? ' ' : '|');
          this->Prefix.push_back(' ');
        }

        FirstChild = true;
        unsigned Depth = Pending.size();

        FC = OrigFC;
        doDumpChild();

        // If any children are left, they're the last at their nesting level.
        // Dump those ones out now.
        while (Depth < Pending.size()) {
          Pending.back()(true);
          this->Pending.pop_back();
        }

        // Restore the old prefix.
        this->Prefix.resize(Prefix.size() - 2);
      };

      if (FirstChild) {
        Pending.push_back(std::move(dumpWithIndent));
      } else {
        Pending.back()(false);
        Pending.back() = std::move(dumpWithIndent);
      }
      FirstChild = false;
    }

    class ColorScope {
      ASTDumper &Dumper;
    public:
      ColorScope(ASTDumper &Dumper, TerminalColor Color)
        : Dumper(Dumper) {
        if (Dumper.ShowColors)
          Dumper.OS.changeColor(Color.Color, Color.Bold);
      }
      ~ColorScope() {
        if (Dumper.ShowColors)
          Dumper.OS.resetColor();
      }
    };

  public:
    ASTDumper(raw_ostream &OS, const CommandTraits *Traits,
              const SourceManager *SM)
      : OS(OS), TopLevel(true), FirstChild(true),
        FC(nullptr),
        ShowColors(SM && SM->getDiagnostics().getShowColors()) { }

    ASTDumper(raw_ostream &OS, const CommandTraits *Traits,
              const SourceManager *SM, bool ShowColors)
      : OS(OS), TopLevel(true), FirstChild(true),
        ShowColors(ShowColors) { }

    void dumpDecl(const Decl *D);
    void dumpStmt(const Stmt *S);

    // Utilities
    // void dumpLocation(SourceLocation Loc);
    void dumpBareType(QualType T, bool Desugar = true);
    void dumpType(QualType T);
    void dumpTypeAsChild(QualType T);
    void dumpTypeAsChild(const Type *T);
    void dumpBareDeclRef(const Decl *Node);
    void dumpDeclRef(const Decl *Node, const char *Label = nullptr);
    void dumpName(const NamedDecl *D);
    bool hasNodes(const DeclContext *DC);
    void dumpDeclContext(const DeclContext *DC);
    void dumpLookups(const DeclContext *DC, bool DumpDecls);
    void dumpAttr(const Attr *A);

    // C++ Utilities
    void dumpAccessSpecifier(AccessSpecifier AS);
    void dumpCXXCtorInitializer(const CXXCtorInitializer *Init);
    void dumpTemplateParameters(const TemplateParameterList *TPL);
    void dumpTemplateArgumentListInfo(const TemplateArgumentListInfo &TALI);
    void dumpTemplateArgumentLoc(const TemplateArgumentLoc &A);
    void dumpTemplateArgumentList(const TemplateArgumentList &TAL);
    void dumpTemplateArgument(const TemplateArgument &A,
                              SourceRange R = SourceRange());

    // Types
    void VisitComplexType(const ComplexType *T) {
      dumpTypeAsChild(T->getElementType());
    }
    void VisitPointerType(const PointerType *T) {
      dumpTypeAsChild(T->getPointeeType());
    }
    void VisitBlockPointerType(const BlockPointerType *T) {
      dumpTypeAsChild(T->getPointeeType());
    }
    void VisitReferenceType(const ReferenceType *T) {
      dumpTypeAsChild(T->getPointeeType());
    }
    void VisitRValueReferenceType(const ReferenceType *T) {
      if (T->isSpelledAsLValue())
        OS << " written as lvalue reference";
      VisitReferenceType(T);
    }
    void VisitMemberPointerType(const MemberPointerType *T) {
      dumpTypeAsChild(T->getClass());
      dumpTypeAsChild(T->getPointeeType());
    }
    void VisitArrayType(const ArrayType *T) {
      switch (T->getSizeModifier()) {
        case ArrayType::Normal: break;
        case ArrayType::Static: OS << " static"; break;
        case ArrayType::Star: OS << " *"; break;
      }
      OS << " " << T->getIndexTypeQualifiers().getAsString();
      dumpTypeAsChild(T->getElementType());
    }
    void VisitConstantArrayType(const ConstantArrayType *T) {
      OS << " " << T->getSize();
      VisitArrayType(T);
    }
    void VisitVariableArrayType(const VariableArrayType *T) {
      OS << " ";
      VisitArrayType(T);
      dumpStmt(T->getSizeExpr());
    }
    void VisitDependentSizedArrayType(const DependentSizedArrayType *T) {
      VisitArrayType(T);
      OS << " ";
      dumpStmt(T->getSizeExpr());
    }
    void VisitDependentSizedExtVectorType(
        const DependentSizedExtVectorType *T) {
      OS << " ";
      dumpTypeAsChild(T->getElementType());
      dumpStmt(T->getSizeExpr());
    }
    void VisitVectorType(const VectorType *T) {
      switch (T->getVectorKind()) {
        case VectorType::GenericVector: break;
        case VectorType::AltiVecVector: OS << " altivec"; break;
        case VectorType::AltiVecPixel: OS << " altivec pixel"; break;
        case VectorType::AltiVecBool: OS << " altivec bool"; break;
        case VectorType::NeonVector: OS << " neon"; break;
        case VectorType::NeonPolyVector: OS << " neon poly"; break;
      }
      OS << " " << T->getNumElements();
      dumpTypeAsChild(T->getElementType());
    }
    void VisitFunctionType(const FunctionType *T) {
      auto EI = T->getExtInfo();
      if (EI.getNoReturn()) OS << " noreturn";
      if (EI.getProducesResult()) OS << " produces_result";
      if (EI.getHasRegParm()) OS << " regparm " << EI.getRegParm();
      OS << " " << FunctionType::getNameForCallConv(EI.getCC());
      dumpTypeAsChild(T->getReturnType());
    }
    void VisitFunctionProtoType(const FunctionProtoType *T) {
      auto EPI = T->getExtProtoInfo();
      if (EPI.HasTrailingReturn) OS << " trailing_return";
      if (T->isConst()) OS << " const";
      if (T->isVolatile()) OS << " volatile";
      if (T->isRestrict()) OS << " restrict";
      switch (EPI.RefQualifier) {
        case RQ_None: break;
        case RQ_LValue: OS << " &"; break;
        case RQ_RValue: OS << " &&"; break;
      }
      // FIXME: Exception specification.
      // FIXME: Consumed parameters.
      VisitFunctionType(T);
      for (QualType PT : T->getParamTypes())
        dumpTypeAsChild(PT);
      if (EPI.Variadic)
        dumpChild([=] { OS << "..."; });
    }
    void VisitUnresolvedUsingType(const UnresolvedUsingType *T) {
      dumpDeclRef(T->getDecl());
    }
    void VisitTypedefType(const TypedefType *T) {
      dumpDeclRef(T->getDecl());
    }
    void VisitTypeOfExprType(const TypeOfExprType *T) {
      dumpStmt(T->getUnderlyingExpr());
    }
    void VisitDecltypeType(const DecltypeType *T) {
      dumpStmt(T->getUnderlyingExpr());
    }
    void VisitUnaryTransformType(const UnaryTransformType *T) {
      switch (T->getUTTKind()) {
      case UnaryTransformType::EnumUnderlyingType:
        OS << " underlying_type";
        break;
      }
      dumpTypeAsChild(T->getBaseType());
    }
    void VisitTagType(const TagType *T) {
      dumpDeclRef(T->getDecl());
    }
    void VisitAttributedType(const AttributedType *T) {
      // FIXME: AttrKind
      dumpTypeAsChild(T->getModifiedType());
    }
    void VisitTemplateTypeParmType(const TemplateTypeParmType *T) {
      OS << " depth " << T->getDepth() << " index " << T->getIndex();
      if (T->isParameterPack()) OS << " pack";
      dumpDeclRef(T->getDecl());
    }
    void VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType *T) {
      dumpTypeAsChild(T->getReplacedParameter());
    }
    void VisitSubstTemplateTypeParmPackType(
        const SubstTemplateTypeParmPackType *T) {
      dumpTypeAsChild(T->getReplacedParameter());
      dumpTemplateArgument(T->getArgumentPack());
    }
    void VisitAutoType(const AutoType *T) {
      if (T->isDecltypeAuto()) OS << " decltype(auto)";
      if (!T->isDeduced())
        OS << " undeduced";
    }
    void VisitTemplateSpecializationType(const TemplateSpecializationType *T) {
      if (T->isTypeAlias()) OS << " alias";
      OS << " "; T->getTemplateName().dump(OS);
      for (auto &Arg : *T)
        dumpTemplateArgument(Arg);
      if (T->isTypeAlias())
        dumpTypeAsChild(T->getAliasedType());
    }
    void VisitInjectedClassNameType(const InjectedClassNameType *T) {
      dumpDeclRef(T->getDecl());
    }
    void VisitAtomicType(const AtomicType *T) {
      dumpTypeAsChild(T->getValueType());
    }
    void VisitAdjustedType(const AdjustedType *T) {
      dumpTypeAsChild(T->getOriginalType());
    }
    void VisitPackExpansionType(const PackExpansionType *T) {
      if (auto N = T->getNumExpansions()) OS << " expansions " << *N;
      if (!T->isSugared())
        dumpTypeAsChild(T->getPattern());
    }
    // FIXME: ElaboratedType, DependentNameType,
    // DependentTemplateSpecializationType, ObjCObjectType

    // Decls
    void VisitLabelDecl(const LabelDecl *D);
    void VisitTypedefDecl(const TypedefDecl *D);
    void VisitEnumDecl(const EnumDecl *D);
    void VisitRecordDecl(const RecordDecl *D);
    void VisitEnumConstantDecl(const EnumConstantDecl *D);
    void VisitIndirectFieldDecl(const IndirectFieldDecl *D);
    void VisitFunctionDecl(const FunctionDecl *D);
    void VisitFieldDecl(const FieldDecl *D);
    void VisitVarDecl(const VarDecl *D);
    void VisitFileScopeAsmDecl(const FileScopeAsmDecl *D);
    void VisitImportDecl(const ImportDecl *D);

    // C++ Decls
    void VisitNamespaceDecl(const NamespaceDecl *D);
    void VisitUsingDirectiveDecl(const UsingDirectiveDecl *D);
    void VisitNamespaceAliasDecl(const NamespaceAliasDecl *D);
    void VisitTypeAliasDecl(const TypeAliasDecl *D);
    void VisitTypeAliasTemplateDecl(const TypeAliasTemplateDecl *D);
    void VisitCXXRecordDecl(const CXXRecordDecl *D);
    void VisitStaticAssertDecl(const StaticAssertDecl *D);
    template<typename SpecializationDecl>
    void VisitTemplateDeclSpecialization(const SpecializationDecl *D,
                                         bool DumpExplicitInst,
                                         bool DumpRefOnly);
    template<typename TemplateDecl>
    void VisitTemplateDecl(const TemplateDecl *D, bool DumpExplicitInst);
    void VisitFunctionTemplateDecl(const FunctionTemplateDecl *D);
    void VisitClassTemplateDecl(const ClassTemplateDecl *D);
    void VisitClassTemplateSpecializationDecl(
        const ClassTemplateSpecializationDecl *D);
    void VisitClassTemplatePartialSpecializationDecl(
        const ClassTemplatePartialSpecializationDecl *D);
    void VisitClassScopeFunctionSpecializationDecl(
        const ClassScopeFunctionSpecializationDecl *D);
    void VisitVarTemplateDecl(const VarTemplateDecl *D);
    void VisitVarTemplateSpecializationDecl(
        const VarTemplateSpecializationDecl *D);
    void VisitVarTemplatePartialSpecializationDecl(
        const VarTemplatePartialSpecializationDecl *D);
    void VisitTemplateTypeParmDecl(const TemplateTypeParmDecl *D);
    void VisitNonTypeTemplateParmDecl(const NonTypeTemplateParmDecl *D);
    void VisitTemplateTemplateParmDecl(const TemplateTemplateParmDecl *D);
    void VisitUsingDecl(const UsingDecl *D);
    void VisitUnresolvedUsingTypenameDecl(const UnresolvedUsingTypenameDecl *D);
    void VisitUnresolvedUsingValueDecl(const UnresolvedUsingValueDecl *D);
    void VisitUsingShadowDecl(const UsingShadowDecl *D);
    void VisitLinkageSpecDecl(const LinkageSpecDecl *D);
    void VisitAccessSpecDecl(const AccessSpecDecl *D);
    void VisitFriendDecl(const FriendDecl *D);

    // Stmts.
    void VisitStmt(const Stmt *Node);
    void VisitDeclStmt(const DeclStmt *Node);
    void VisitAttributedStmt(const AttributedStmt *Node);
    void VisitLabelStmt(const LabelStmt *Node);
    void VisitGotoStmt(const GotoStmt *Node);
    void VisitCXXCatchStmt(const CXXCatchStmt *Node);
  };
}

//===----------------------------------------------------------------------===//
//  Utilities
//===----------------------------------------------------------------------===//

void ASTDumper::dumpBareType(QualType T, bool Desugar) {
  ColorScope Color(*this, TypeColor);

  SplitQualType T_split = T.split();
  OS << "'" << QualType::getAsString(T_split) << "'";

  if (Desugar && !T.isNull()) {
    // If the type is sugared, also dump a (shallow) desugared type.
    SplitQualType D_split = T.getSplitDesugaredType();
    if (T_split != D_split)
      OS << ":'" << QualType::getAsString(D_split) << "'";
  }
}

void ASTDumper::dumpType(QualType T) {
  OS << ' ';
  dumpBareType(T);
}

void ASTDumper::dumpTypeAsChild(QualType T) {
  SplitQualType SQT = T.split();
  if (!SQT.Quals.hasQualifiers())
    return dumpTypeAsChild(SQT.Ty);

  dumpChild([=] {
    OS << "QualType";
    OS << " ";
    dumpBareType(T, false);
    OS << " " << T.split().Quals.getAsString();
    dumpTypeAsChild(T.split().Ty);
  });
}

void ASTDumper::dumpTypeAsChild(const Type *T) {
  dumpChild([=] {
    if (!T) {
      ColorScope Color(*this, NullColor);
      OS << "<<<NULL>>>";
      return;
    }

    {
      ColorScope Color(*this, TypeColor);
      OS << T->getTypeClassName() << "Type";
    }
    OS << " ";
    dumpBareType(QualType(T, 0), false);

    QualType SingleStepDesugar =
        T->getLocallyUnqualifiedSingleStepDesugaredType();
    if (SingleStepDesugar != QualType(T, 0))
      OS << " sugar";
    if (T->isDependentType())
      OS << " dependent";
    else if (T->isInstantiationDependentType())
      OS << " instantiation_dependent";
    if (T->isVariablyModifiedType())
      OS << " variably_modified";
    if (T->containsUnexpandedParameterPack())
      OS << " contains_unexpanded_pack";
    if (T->isFromAST())
      OS << " imported";

    TypeVisitor<ASTDumper>::Visit(T);

    if (SingleStepDesugar != QualType(T, 0))
      dumpTypeAsChild(SingleStepDesugar);
  });
}

void ASTDumper::dumpBareDeclRef(const Decl *D) {
  {
    ColorScope Color(*this, DeclKindNameColor);
    OS << D->getDeclKindName();
  }

  if (const NamedDecl *ND = dyn_cast<NamedDecl>(D)) {
    ColorScope Color(*this, DeclNameColor);
    OS << " '" << ND->getDeclName() << '\'';
  }

  if (const ValueDecl *VD = dyn_cast<ValueDecl>(D))
    dumpType(VD->getType());
}

void ASTDumper::dumpDeclRef(const Decl *D, const char *Label) {
  if (!D)
    return;

  dumpChild([=]{
    if (Label)
      OS << Label << ' ';
    dumpBareDeclRef(D);
  });
}

void ASTDumper::dumpName(const NamedDecl *ND) {
  if (ND->getDeclName()) {
    ColorScope Color(*this, DeclNameColor);
    OS << ' ' << ND->getNameAsString();
  }
}

bool ASTDumper::hasNodes(const DeclContext *DC) {
  if (!DC)
    return false;

  return DC->hasExternalLexicalStorage() ||
         DC->noload_decls_begin() != DC->noload_decls_end();
}

void ASTDumper::dumpDeclContext(const DeclContext *DC) {
  if (!DC)
    return;

  for (auto *D : DC->noload_decls())
    dumpDecl(D);

  if (DC->hasExternalLexicalStorage()) {
    dumpChild([=]{
      ColorScope Color(*this, UndeserializedColor);
      OS << "<undeserialized declarations>";
    });
  }
}

void ASTDumper::dumpLookups(const DeclContext *DC, bool DumpDecls) {
  dumpChild([=] {
    OS << "StoredDeclsMap ";
    dumpBareDeclRef(cast<Decl>(DC));

    const DeclContext *Primary = DC->getPrimaryContext();
    if (Primary != DC) {
      OS << " primary";
    }

    bool HasUndeserializedLookups = Primary->hasExternalVisibleStorage();

    DeclContext::all_lookups_iterator I = Primary->noload_lookups_begin(),
                                      E = Primary->noload_lookups_end();
    while (I != E) {
      DeclarationName Name = I.getLookupName();
      DeclContextLookupResult R = *I++;

      dumpChild([=] {
        OS << "DeclarationName ";
        {
          ColorScope Color(*this, DeclNameColor);
          OS << '\'' << Name << '\'';
        }

        for (DeclContextLookupResult::iterator RI = R.begin(), RE = R.end();
             RI != RE; ++RI) {
          dumpChild([=] {
            dumpBareDeclRef(*RI);

            if ((*RI)->isHidden())
              OS << " hidden";

            // If requested, dump the redecl chain for this lookup.
            if (DumpDecls) {
              // Dump earliest decl first.
              std::function<void(Decl *)> DumpWithPrev = [&](Decl *D) {
                if (Decl *Prev = D->getPreviousDecl())
                  DumpWithPrev(Prev);
                dumpDecl(D);
              };
              DumpWithPrev(*RI);
            }
          });
        }
      });
    }

    if (HasUndeserializedLookups) {
      dumpChild([=] {
        ColorScope Color(*this, UndeserializedColor);
        OS << "<undeserialized lookups>";
      });
    }
  });
}

void ASTDumper::dumpAttr(const Attr *A) {
  dumpChild([=] {
    {
      ColorScope Color(*this, AttrColor);

      switch (A->getKind()) {
#define ATTR(X) case attr::X: OS << #X; break;
#include "clang/Basic/AttrList.inc"
      default:
        llvm_unreachable("unexpected attribute kind");
      }
      OS << "Attr";
    }
    if (A->isInherited())
      OS << " Inherited";
    if (A->isImplicit())
      OS << " Implicit";
#include "clang/AST/AttrDump.inc"
  });
}

static void dumpPreviousDeclImpl(raw_ostream &OS, ...) {}

template<typename T>
static void dumpPreviousDeclImpl(raw_ostream &OS, const Mergeable<T> *D) {
  const T *First = D->getFirstDecl();
  if (First != D)
    OS << " first " << First;
}

template<typename T>
static void dumpPreviousDeclImpl(raw_ostream &OS, const Redeclarable<T> *D) {
  const T *Prev = D->getPreviousDecl();
  if (Prev)
    OS << " prev " << Prev;
}

/// Dump the previous declaration in the redeclaration chain for a declaration,
/// if any.
static void dumpPreviousDecl(raw_ostream &OS, const Decl *D) {
  switch (D->getKind()) {
#define DECL(DERIVED, BASE) \
  case Decl::DERIVED: \
    return dumpPreviousDeclImpl(OS, cast<DERIVED##Decl>(D));
#define ABSTRACT_DECL(DECL)
#include "clang/AST/DeclNodes.inc"
  }
  llvm_unreachable("Decl that isn't part of DeclNodes.inc!");
}

//===----------------------------------------------------------------------===//
//  C++ Utilities
//===----------------------------------------------------------------------===//

void ASTDumper::dumpAccessSpecifier(AccessSpecifier AS) {
  switch (AS) {
  case AS_none:
    break;
  case AS_public:
    OS << "public";
    break;
  case AS_protected:
    OS << "protected";
    break;
  case AS_private:
    OS << "private";
    break;
  }
}

void ASTDumper::dumpCXXCtorInitializer(const CXXCtorInitializer *Init) {
  dumpChild([=] {
    OS << "CXXCtorInitializer";
    if (Init->isAnyMemberInitializer()) {
      OS << ' ';
      dumpBareDeclRef(Init->getAnyMember());
    } else if (Init->isBaseInitializer()) {
      dumpType(QualType(Init->getBaseClass(), 0));
    } else if (Init->isDelegatingInitializer()) {
      dumpType(Init->getTypeSourceInfo()->getType());
    } else {
      llvm_unreachable("Unknown initializer type");
    }
    dumpStmt(Init->getInit());
  });
}

void ASTDumper::dumpTemplateParameters(const TemplateParameterList *TPL) {
  if (!TPL)
    return;

  for (TemplateParameterList::const_iterator I = TPL->begin(), E = TPL->end();
       I != E; ++I)
    dumpDecl(*I);
}

void ASTDumper::dumpTemplateArgumentListInfo(
    const TemplateArgumentListInfo &TALI) {
  for (unsigned i = 0, e = TALI.size(); i < e; ++i)
    dumpTemplateArgumentLoc(TALI[i]);
}

void ASTDumper::dumpTemplateArgumentLoc(const TemplateArgumentLoc &A) {
  dumpTemplateArgument(A.getArgument(), A.getSourceRange());
}

void ASTDumper::dumpTemplateArgumentList(const TemplateArgumentList &TAL) {
  for (unsigned i = 0, e = TAL.size(); i < e; ++i)
    dumpTemplateArgument(TAL[i]);
}

void ASTDumper::dumpTemplateArgument(const TemplateArgument &A, SourceRange R) {
  dumpChild([=] {
    OS << "TemplateArgument";

    switch (A.getKind()) {
    case TemplateArgument::Null:
      OS << " null";
      break;
    case TemplateArgument::Type:
      OS << " type";
      dumpType(A.getAsType());
      break;
    case TemplateArgument::Declaration:
      OS << " decl";
      dumpDeclRef(A.getAsDecl());
      break;
    case TemplateArgument::NullPtr:
      OS << " nullptr";
      break;
    case TemplateArgument::Integral:
      OS << " integral " << A.getAsIntegral();
      break;
    case TemplateArgument::Template:
      OS << " template ";
      A.getAsTemplate().dump(OS);
      break;
    case TemplateArgument::TemplateExpansion:
      OS << " template expansion";
      A.getAsTemplateOrTemplatePattern().dump(OS);
      break;
    case TemplateArgument::Expression:
      OS << " expr";
      dumpStmt(A.getAsExpr());
      break;
    case TemplateArgument::Pack:
      OS << " pack";
      for (TemplateArgument::pack_iterator I = A.pack_begin(), E = A.pack_end();
           I != E; ++I)
        dumpTemplateArgument(*I);
      break;
    }
  });
}

//===----------------------------------------------------------------------===//
//  Decl dumping methods.
//===----------------------------------------------------------------------===//

void ASTDumper::dumpDecl(const Decl *D) {
  dumpChild([=] {
    if (!D) {
      ColorScope Color(*this, NullColor);
      OS << "<<<NULL>>>";
      return;
    }

    {
      ColorScope Color(*this, DeclKindNameColor);
      OS << D->getDeclKindName() << "Decl";
    }
    if (D->getLexicalDeclContext() != D->getDeclContext())
      OS << " parent " << cast<Decl>(D->getDeclContext());
    dumpPreviousDecl(OS, D);
    OS << ' ';
    if (Module *M = D->getOwningModule())
      OS << " in " << M->getFullModuleName();
    if (const NamedDecl *ND = dyn_cast<NamedDecl>(D))
      if (ND->isHidden())
        OS << " hidden";
    if (D->isImplicit())
      OS << " implicit";
    if (D->isUsed())
      OS << " used";
    else if (D->isThisDeclarationReferenced())
      OS << " referenced";
    if (D->isInvalidDecl())
      OS << " invalid";
    if (const FunctionDecl *FD = dyn_cast<FunctionDecl>(D))
      if (FD->isConstexpr())
        OS << " constexpr";


    ConstDeclVisitor<ASTDumper>::Visit(D);

    for (Decl::attr_iterator I = D->attr_begin(), E = D->attr_end(); I != E;
         ++I)
      dumpAttr(*I);

    // if (const FullComment *Comment =
    //         D->getASTContext().getLocalCommentForDeclUncached(D))
    //   dumpFullComment(Comment);

    // Decls within functions are visited by the body.
    if (!isa<FunctionDecl>(*D) && !isa<ObjCMethodDecl>(*D) &&
        hasNodes(dyn_cast<DeclContext>(D)))
      dumpDeclContext(cast<DeclContext>(D));
  });
}

void ASTDumper::VisitLabelDecl(const LabelDecl *D) {
  dumpName(D);
}

void ASTDumper::VisitTypedefDecl(const TypedefDecl *D) {
  dumpName(D);
  dumpType(D->getUnderlyingType());
  if (D->isModulePrivate())
    OS << " __module_private__";
}

void ASTDumper::VisitEnumDecl(const EnumDecl *D) {
  if (D->isScoped()) {
    if (D->isScopedUsingClassTag())
      OS << " class";
    else
      OS << " struct";
  }
  dumpName(D);
  if (D->isModulePrivate())
    OS << " __module_private__";
  if (D->isFixed())
    dumpType(D->getIntegerType());
}

void ASTDumper::VisitRecordDecl(const RecordDecl *D) {
  OS << ' ' << D->getKindName();
  dumpName(D);
  if (D->isModulePrivate())
    OS << " __module_private__";
  if (D->isCompleteDefinition())
    OS << " definition";
}

void ASTDumper::VisitEnumConstantDecl(const EnumConstantDecl *D) {
  dumpName(D);
  dumpType(D->getType());
  if (const Expr *Init = D->getInitExpr())
    dumpStmt(Init);
}

void ASTDumper::VisitIndirectFieldDecl(const IndirectFieldDecl *D) {
  dumpName(D);
  dumpType(D->getType());

  for (auto *Child : D->chain())
    dumpDeclRef(Child);
}

void ASTDumper::VisitFunctionDecl(const FunctionDecl *D) {
  dumpName(D);
  dumpType(D->getType());

  StorageClass SC = D->getStorageClass();
  if (SC != SC_None)
    OS << ' ' << VarDecl::getStorageClassSpecifierString(SC);
  if (D->isInlineSpecified())
    OS << " inline";
  if (D->isVirtualAsWritten())
    OS << " virtual";
  if (D->isModulePrivate())
    OS << " __module_private__";

  if (D->isPure())
    OS << " pure";
  else if (D->isDeletedAsWritten())
    OS << " delete";

  if (const FunctionProtoType *FPT = D->getType()->getAs<FunctionProtoType>()) {
    FunctionProtoType::ExtProtoInfo EPI = FPT->getExtProtoInfo();
    switch (EPI.ExceptionSpec.Type) {
    default: break;
    case EST_Unevaluated:
      OS << " noexcept-unevaluated " << EPI.ExceptionSpec.SourceDecl;
      break;
    case EST_Uninstantiated:
      OS << " noexcept-uninstantiated " << EPI.ExceptionSpec.SourceTemplate;
      break;
    }
  }

  if (const FunctionTemplateSpecializationInfo *FTSI =
          D->getTemplateSpecializationInfo())
    dumpTemplateArgumentList(*FTSI->TemplateArguments);

  for (ArrayRef<NamedDecl *>::iterator
       I = D->getDeclsInPrototypeScope().begin(),
       E = D->getDeclsInPrototypeScope().end(); I != E; ++I)
    dumpDecl(*I);

  for (FunctionDecl::param_const_iterator I = D->param_begin(),
                                          E = D->param_end();
       I != E; ++I)
    dumpDecl(*I);

  if (const CXXConstructorDecl *C = dyn_cast<CXXConstructorDecl>(D))
    for (CXXConstructorDecl::init_const_iterator I = C->init_begin(),
                                                 E = C->init_end();
         I != E; ++I)
      dumpCXXCtorInitializer(*I);

  if (D->doesThisDeclarationHaveABody())
    dumpStmt(D->getBody());
}

void ASTDumper::VisitFieldDecl(const FieldDecl *D) {
  dumpName(D);
  dumpType(D->getType());
  if (D->isMutable())
    OS << " mutable";
  if (D->isModulePrivate())
    OS << " __module_private__";

  if (D->isBitField())
    dumpStmt(D->getBitWidth());
  if (Expr *Init = D->getInClassInitializer())
    dumpStmt(Init);
}

void ASTDumper::VisitVarDecl(const VarDecl *D) {
  dumpName(D);
  dumpType(D->getType());
  StorageClass SC = D->getStorageClass();
  if (SC != SC_None)
    OS << ' ' << VarDecl::getStorageClassSpecifierString(SC);
  switch (D->getTLSKind()) {
  case VarDecl::TLS_None: break;
  case VarDecl::TLS_Static: OS << " tls"; break;
  case VarDecl::TLS_Dynamic: OS << " tls_dynamic"; break;
  }
  if (D->isModulePrivate())
    OS << " __module_private__";
  if (D->isNRVOVariable())
    OS << " nrvo";
  if (D->hasInit()) {
    switch (D->getInitStyle()) {
    case VarDecl::CInit: OS << " cinit"; break;
    case VarDecl::CallInit: OS << " callinit"; break;
    case VarDecl::ListInit: OS << " listinit"; break;
    }
    dumpStmt(D->getInit());
  }
}

void ASTDumper::VisitFileScopeAsmDecl(const FileScopeAsmDecl *D) {
  dumpStmt(D->getAsmString());
}

void ASTDumper::VisitImportDecl(const ImportDecl *D) {
  OS << ' ' << D->getImportedModule()->getFullModuleName();
}

//===----------------------------------------------------------------------===//
// C++ Declarations
//===----------------------------------------------------------------------===//

void ASTDumper::VisitNamespaceDecl(const NamespaceDecl *D) {
  dumpName(D);
  if (D->isInline())
    OS << " inline";
  if (!D->isOriginalNamespace())
    dumpDeclRef(D->getOriginalNamespace(), "original");
}

void ASTDumper::VisitUsingDirectiveDecl(const UsingDirectiveDecl *D) {
  OS << ' ';
  dumpBareDeclRef(D->getNominatedNamespace());
}

void ASTDumper::VisitNamespaceAliasDecl(const NamespaceAliasDecl *D) {
  dumpName(D);
  dumpDeclRef(D->getAliasedNamespace());
}

void ASTDumper::VisitTypeAliasDecl(const TypeAliasDecl *D) {
  dumpName(D);
  dumpType(D->getUnderlyingType());
}

void ASTDumper::VisitTypeAliasTemplateDecl(const TypeAliasTemplateDecl *D) {
  dumpName(D);
  dumpTemplateParameters(D->getTemplateParameters());
  dumpDecl(D->getTemplatedDecl());
}

void ASTDumper::VisitCXXRecordDecl(const CXXRecordDecl *D) {
  VisitRecordDecl(D);
  if (!D->isCompleteDefinition())
    return;

  for (const auto &I : D->bases()) {
    dumpChild([=] {
      if (I.isVirtual())
        OS << "virtual ";
      dumpAccessSpecifier(I.getAccessSpecifier());
      dumpType(I.getType());
      if (I.isPackExpansion())
        OS << "...";
    });
  }
}

void ASTDumper::VisitStaticAssertDecl(const StaticAssertDecl *D) {
  dumpStmt(D->getAssertExpr());
  dumpStmt(D->getMessage());
}

template<typename SpecializationDecl>
void ASTDumper::VisitTemplateDeclSpecialization(const SpecializationDecl *D,
                                                bool DumpExplicitInst,
                                                bool DumpRefOnly) {
  bool DumpedAny = false;
  for (auto *RedeclWithBadType : D->redecls()) {
    // FIXME: The redecls() range sometimes has elements of a less-specific
    // type. (In particular, ClassTemplateSpecializationDecl::redecls() gives
    // us TagDecls, and should give CXXRecordDecls).
    auto *Redecl = dyn_cast<SpecializationDecl>(RedeclWithBadType);
    if (!Redecl) {
      // Found the injected-class-name for a class template. This will be dumped
      // as part of its surrounding class so we don't need to dump it here.
      assert(isa<CXXRecordDecl>(RedeclWithBadType) &&
             "expected an injected-class-name");
      continue;
    }

    switch (Redecl->getTemplateSpecializationKind()) {
    case TSK_ExplicitInstantiationDeclaration:
    case TSK_ExplicitInstantiationDefinition:
      if (!DumpExplicitInst)
        break;
      // Fall through.
    case TSK_Undeclared:
    case TSK_ImplicitInstantiation:
      if (DumpRefOnly)
        dumpDeclRef(Redecl);
      else
        dumpDecl(Redecl);
      DumpedAny = true;
      break;
    case TSK_ExplicitSpecialization:
      break;
    }
  }

  // Ensure we dump at least one decl for each specialization.
  if (!DumpedAny)
    dumpDeclRef(D);
}

template<typename TemplateDecl>
void ASTDumper::VisitTemplateDecl(const TemplateDecl *D,
                                  bool DumpExplicitInst) {
  dumpName(D);
  dumpTemplateParameters(D->getTemplateParameters());

  dumpDecl(D->getTemplatedDecl());

  for (auto *Child : D->specializations())
    VisitTemplateDeclSpecialization(Child, DumpExplicitInst,
                                    !D->isCanonicalDecl());
}

void ASTDumper::VisitFunctionTemplateDecl(const FunctionTemplateDecl *D) {
  // FIXME: We don't add a declaration of a function template specialization
  // to its context when it's explicitly instantiated, so dump explicit
  // instantiations when we dump the template itself.
  VisitTemplateDecl(D, true);
}

void ASTDumper::VisitClassTemplateDecl(const ClassTemplateDecl *D) {
  VisitTemplateDecl(D, false);
}

void ASTDumper::VisitClassTemplateSpecializationDecl(
    const ClassTemplateSpecializationDecl *D) {
  VisitCXXRecordDecl(D);
  dumpTemplateArgumentList(D->getTemplateArgs());
}

void ASTDumper::VisitClassTemplatePartialSpecializationDecl(
    const ClassTemplatePartialSpecializationDecl *D) {
  VisitClassTemplateSpecializationDecl(D);
  dumpTemplateParameters(D->getTemplateParameters());
}

void ASTDumper::VisitClassScopeFunctionSpecializationDecl(
    const ClassScopeFunctionSpecializationDecl *D) {
  dumpDeclRef(D->getSpecialization());
  if (D->hasExplicitTemplateArgs())
    dumpTemplateArgumentListInfo(D->templateArgs());
}

void ASTDumper::VisitVarTemplateDecl(const VarTemplateDecl *D) {
  VisitTemplateDecl(D, false);
}

void ASTDumper::VisitVarTemplateSpecializationDecl(
    const VarTemplateSpecializationDecl *D) {
  dumpTemplateArgumentList(D->getTemplateArgs());
  VisitVarDecl(D);
}

void ASTDumper::VisitVarTemplatePartialSpecializationDecl(
    const VarTemplatePartialSpecializationDecl *D) {
  dumpTemplateParameters(D->getTemplateParameters());
  VisitVarTemplateSpecializationDecl(D);
}

void ASTDumper::VisitTemplateTypeParmDecl(const TemplateTypeParmDecl *D) {
  if (D->wasDeclaredWithTypename())
    OS << " typename";
  else
    OS << " class";
  if (D->isParameterPack())
    OS << " ...";
  dumpName(D);
  if (D->hasDefaultArgument())
    dumpTemplateArgument(D->getDefaultArgument());
}

void ASTDumper::VisitNonTypeTemplateParmDecl(const NonTypeTemplateParmDecl *D) {
  dumpType(D->getType());
  if (D->isParameterPack())
    OS << " ...";
  dumpName(D);
  if (D->hasDefaultArgument())
    dumpTemplateArgument(D->getDefaultArgument());
}

void ASTDumper::VisitTemplateTemplateParmDecl(
    const TemplateTemplateParmDecl *D) {
  if (D->isParameterPack())
    OS << " ...";
  dumpName(D);
  dumpTemplateParameters(D->getTemplateParameters());
  if (D->hasDefaultArgument())
    dumpTemplateArgumentLoc(D->getDefaultArgument());
}

void ASTDumper::VisitUsingDecl(const UsingDecl *D) {
  OS << ' ';
  D->getQualifier()->print(OS, D->getASTContext().getPrintingPolicy());
  OS << D->getNameAsString();
}

void ASTDumper::VisitUnresolvedUsingTypenameDecl(
    const UnresolvedUsingTypenameDecl *D) {
  OS << ' ';
  D->getQualifier()->print(OS, D->getASTContext().getPrintingPolicy());
  OS << D->getNameAsString();
}

void ASTDumper::VisitUnresolvedUsingValueDecl(const UnresolvedUsingValueDecl *D) {
  OS << ' ';
  D->getQualifier()->print(OS, D->getASTContext().getPrintingPolicy());
  OS << D->getNameAsString();
  dumpType(D->getType());
}

void ASTDumper::VisitUsingShadowDecl(const UsingShadowDecl *D) {
  OS << ' ';
  dumpBareDeclRef(D->getTargetDecl());
}

void ASTDumper::VisitLinkageSpecDecl(const LinkageSpecDecl *D) {
  switch (D->getLanguage()) {
  case LinkageSpecDecl::lang_c: OS << " C"; break;
  case LinkageSpecDecl::lang_cxx: OS << " C++"; break;
  }
}

void ASTDumper::VisitAccessSpecDecl(const AccessSpecDecl *D) {
  OS << ' ';
  dumpAccessSpecifier(D->getAccess());
}

void ASTDumper::VisitFriendDecl(const FriendDecl *D) {
  if (TypeSourceInfo *T = D->getFriendType())
    dumpType(T->getType());
  else
    dumpDecl(D->getFriendDecl());
}


//===----------------------------------------------------------------------===//
//  Stmt dumping methods.
//===----------------------------------------------------------------------===//

void ASTDumper::dumpStmt(const Stmt *S) {
  dumpChild([=] {
    if (!S) {
      ColorScope Color(*this, NullColor);
      OS << "<<<NULL>>>";
      return;
    }

    if (const DeclStmt *DS = dyn_cast<DeclStmt>(S)) {
      VisitDeclStmt(DS);
      return;
    }

    ConstStmtVisitor<ASTDumper>::Visit(S);

    for (Stmt::const_child_range CI = S->children(); CI; ++CI)
      dumpStmt(*CI);
  });
}

void ASTDumper::VisitStmt(const Stmt *Node) {
  {
    ColorScope Color(*this, StmtColor);
    OS << Node->getStmtClassName();
  }
}

void ASTDumper::VisitDeclStmt(const DeclStmt *Node) {
  VisitStmt(Node);
  for (DeclStmt::const_decl_iterator I = Node->decl_begin(),
                                     E = Node->decl_end();
       I != E; ++I)
    dumpDecl(*I);
}

void ASTDumper::VisitAttributedStmt(const AttributedStmt *Node) {
  VisitStmt(Node);
  for (ArrayRef<const Attr *>::iterator I = Node->getAttrs().begin(),
                                        E = Node->getAttrs().end();
       I != E; ++I)
    dumpAttr(*I);
}

void ASTDumper::VisitLabelStmt(const LabelStmt *Node) {
  VisitStmt(Node);
  OS << " '" << Node->getName() << "'";
}

void ASTDumper::VisitGotoStmt(const GotoStmt *Node) {
  VisitStmt(Node);
  OS << " '" << Node->getLabel()->getName() << "'";
}

void ASTDumper::VisitCXXCatchStmt(const CXXCatchStmt *Node) {
  VisitStmt(Node);
  dumpDecl(Node->getExceptionDecl());
}






  class PrintFunctionsConsumer : public ASTConsumer,
                     public RecursiveASTVisitor<PrintFunctionsConsumer> {
    typedef RecursiveASTVisitor<PrintFunctionsConsumer> base;

  public:
    PrintFunctionsConsumer(StringRef FilterString = "ns3::ndn", bool DumpLookups = false)
        : Out(llvm::outs()),
          FilterString(FilterString) {}

    void HandleTranslationUnit(ASTContext &Context) override {
      TranslationUnitDecl *D = Context.getTranslationUnitDecl();

      if (FilterString.empty())
        return print(D);

      TraverseDecl(D);
    }

    // bool shouldWalkTypesOfTypeLocs() const { return false; }

    bool TraverseDecl(Decl *D) {
      if (D && filterMatches(D)) {
        print(D);
        // Don't traverse child nodes to avoid output duplication.
        return true;
      }
      return base::TraverseDecl(D);
    }

  private:
    std::string getName(Decl *D) {
      if (isa<NamedDecl>(D))
        return cast<NamedDecl>(D)->getQualifiedNameAsString();
      return "";
    }
    bool filterMatches(Decl *D) {
      return getName(D).find(FilterString) != std::string::npos;
    }
    void print(Decl *D) {
      // if (true) {
      //   if (DeclContext *DC = dyn_cast<DeclContext>(D)) {
      //     if (DC == DC->getPrimaryContext())
      //       DC->dumpLookups(Out, true);
      //     else
      //       Out << "Lookup map is in primary DeclContext "
      //           << DC->getPrimaryContext() << "\n";
      //   } else
      //     Out << "Not a DeclContext\n";
      // } else if (false)
      ASTDumper P(llvm::errs(), &D->getASTContext().getCommentCommandTraits(),
                  &D->getASTContext().getSourceManager(), /*ShowColors*/true);
      P.dumpDecl(D);

      // D->dumpColor();
      // else
      // D->print(Out, /*Indentation=*/0, /*PrintInstantiation=*/true);
    }

    raw_ostream &Out;
    std::string FilterString;
  };
















class PrintFunctionNamesAction : public PluginASTAction {
protected:
  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 llvm::StringRef) {
    return llvm::make_unique<PrintFunctionsConsumer>();
  }

  bool ParseArgs(const CompilerInstance &CI,
                 const std::vector<std::string>& args) {
    for (unsigned i = 0, e = args.size(); i != e; ++i) {
      llvm::errs() << "PrintFunctionNames arg = " << args[i] << "\n";

      // Example error handling.
      if (args[i] == "-an-error") {
        DiagnosticsEngine &D = CI.getDiagnostics();
        unsigned DiagID = D.getCustomDiagID(DiagnosticsEngine::Error,
                                            "invalid argument '%0'");
        D.Report(DiagID) << args[i];
        return false;
      }
    }
    if (args.size() && args[0] == "help")
      PrintHelp(llvm::errs());

    return true;
  }
  void PrintHelp(llvm::raw_ostream& ros) {
    ros << "Help for PrintFunctionNames plugin goes here\n";
  }

};

// }

static FrontendPluginRegistry::Add<PrintFunctionNamesAction>
X("print-fns", "print function names");
