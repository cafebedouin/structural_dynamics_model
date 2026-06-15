#!/usr/bin/env python3
"""Assemble judge_results.json from the LLM-judge's per-omega reasoned verdicts.

The verdicts below are produced by READING each omega's (question/approach/consequence)
and declared fields and applying signature-detection + the external-vs-restatement gate
by REASONING (not lexical matching). Each entry cites the omega's own text and which
declared field made the locus internal/external. This script only joins those hand-authored
verdicts to the sample and computes the vs_authored field + summary tables.
"""
import json, os, collections

HERE = os.path.dirname(os.path.abspath(__file__))
SAMPLE = os.path.join(HERE, "sample_40.json")
OUT = os.path.join(HERE, "judge_results.json")

# diagnosis: Ω_C | Ω_P | Ω_E | hybrid(...) | restatement
# external_to: criterion | decider | observation | none  (multi: comma-join)
# fired: subset of [define, decide, measure]
# Each verdict: (fired, external_to, diagnosis, confidence, rationale)
V = {
 0: (["define"], "criterion", "Ω_C", "high",
     "Asks whether the overdetermination reading is a substantive reading OF the texts or a "
     "meta-reading ABOUT readings; resolution is hermeneutical clarification of a term left "
     "OPEN by the declared set (composite_overdetermination_reading coexists_with both "
     "siblings but its kind is unspecified). define fires; criterion external -> Ω_C. "
     "'measure' echo ('cite Vatican II passages') is in service of the definition, not an "
     "unobserved external fact."),
 1: (["measure"], "observation", "Ω_E", "high",
     "Asks which medical facts genuinely depend on biological sex via 'systematic medical "
     "evidence review' of causal mechanisms — an unobserved external empirical fact not "
     "re-derivable from declared fields. measure external -> Ω_E. Authored empirical: agree."),
 2: (["measure"], "observation", "Ω_E", "high",
     "Whether practitioners held domain-partition logic is settled by 'textual analysis of "
     "practitioner statements (diaries, confessions) pre/post-Meiji' — external historical "
     "observation. measure external -> Ω_E. Authored conceptual; the resolution is empirical "
     "not definitional (a fact about what practitioners believed)."),
 3: (["decide"], "decider", "Ω_P", "high",
     "'Should plural wives have had formal voice?' The deciding locus is a value about whose "
     "consent the revelation mechanism owes — a stakeholder/value not contained by the "
     "constraint (beneficiary=institutional_church_leadership, victims include female_plural_"
     "wives, but no authored field settles whether their voice is owed). The 'examine other "
     "contexts' approach serves the value question. decide external -> Ω_P. Authored: agree."),
 4: (["measure"], "observation", "Ω_E", "high",
     "Whether the rupture reading is textually grounded or constructed post-hoc -> 'systematic "
     "comparative exegesis ... analysis of conciliar-process documents (interventions, voting "
     "patterns)' — external historical observation. measure external -> Ω_E. Authored: agree."),
 5: (["measure"], "observation", "Ω_E", "high",
     "Whether alternative security arrangements were proposed/rejected -> 'historical record of "
     "negotiations (Camp David, Taba, ...), documentary analysis'. External observation. "
     "measure external -> Ω_E. Authored: agree."),
 6: (["measure"], "observation", "Ω_E", "high",
     "Structural vs internalized suppression -> 'historiography of journals, hiring, citation "
     "patterns; count adherents by decade'. External observation. measure -> Ω_E. Authored: agree."),
 7: (["define"], "criterion", "Ω_C", "medium",
     "FAMILY. 'Could this constraint be read under a DIFFERENT kernel that yields different ε?' "
     "Resolution tests alternative kernels (framework selection) against the declared kernel "
     "dollar_gold_convertibility. This is a define/framework-selection at criterion-locus: the "
     "alternative kernel is NOT in the declared set -> external term -> Ω_C. The ε mention is a "
     "consequence of reframing, not an authored-ε re-derivation. Authored conceptual: agree."),
 8: (["measure"], "observation", "Ω_E", "high",
     "Are women absent or present-but-unheard -> 'ethnographic/qualitative research, documentation "
     "of women's arguments'. External observation. measure -> Ω_E. Authored empirical: agree."),
 9: (["define"], "criterion", "Ω_C", "high",
     "FAMILY. Does biomedical premise logically foreclose the critical premise, or coexist? "
     "Resolution is 'conceptual analysis' of axiom compatibility — a framework/criterion call "
     "over the declared readings (biomedical/critical/market). The foreclose-vs-coexist relation "
     "is exactly what cs_reading_relation leaves to be defined. define external -> Ω_C. Agree."),
 10: (["define"], "criterion", "Ω_C", "high",
     "FAMILY. Does public_health reading foreclose bodily_autonomy, or are they irreconcilable "
     "commitments? 'Jurisprudential analysis ... is sustained coexistence coherent?' This is a "
     "conceptual foreclosure/coexistence determination over declared siblings; criterion "
     "(what 'foreclose' requires) is the open term. define external -> Ω_C. Agree."),
 11: (["define"], "criterion", "Ω_C", "high",
     "FAMILY. Does degrowth premise foreclose mitigation+adaptation? 'Logical analysis of axiom "
     "compatibility'. Conceptual foreclosure call over the declared triad. define -> Ω_C. Agree."),
 12: (["measure"], "observation", "Ω_E", "high",
     "Epistemic belief vs sunk-cost/reputational lock -> 'structural interview or revealed-"
     "preference experiment'. External observation of actors' motives. measure -> Ω_E. Authored "
     "empirical: agree."),
 13: (["measure"], "observation", "Ω_E", "high",
     "Natural linguistic change vs corruption -> 'comparative study of documented language change "
     "in other families; historical linguistics reconstruction'. External observation. measure -> "
     "Ω_E. Authored empirical: agree."),
 14: (["measure"], "observation", "Ω_E", "high",
     "Can carbon removal scale to gigatons/yr energy-neutrally -> 'real-world deployment data by "
     "2030 (DAC, weathering, biochar thresholds)'. External observation. measure -> Ω_E. Agree."),
 15: (["decide", "measure"], "decider", "Ω_P", "medium",
     "PREFERENCE. 'Is the dead victim genuinely benefited, or is benefit transferred to state/"
     "family with the victim narrated as beneficiary?' The deciding locus is whether a deceased "
     "person can be benefited — a value/normative judgment (deontological axiom) about beneficiary "
     "legitimacy, not settled by the authored beneficiary field. 'Interview families' (measure) is "
     "evidentiary support but the DECIDER is the value premise. decide external (decider) -> Ω_P. "
     "Authored preference: agree."),
 16: (["define"], "criterion", "Ω_C", "high",
     "'Is bodily autonomy categorical or prima facie?' Resolution is 'philosophical analysis' "
     "specifying which definition of the right applies — open criterion, not in declared fields. "
     "The 'case-law evolution' clause is illustration of consequences, not the deciding observation. "
     "define external -> Ω_C. Authored conceptual: agree."),
 17: (["define"], "criterion", "Ω_C", "high",
     "FAMILY. Is bodily-autonomy reading one reading of the SAME kernel, or a different kernel? "
     "'Conceptual analysis of kernel definition: do the three readings adjudicate the SAME "
     "question?' Criterion (kernel identity) is the open term over the declared cs_kernel_id. "
     "define external -> Ω_C. Authored conceptual: agree."),
 18: (["measure"], "observation", "Ω_E", "high",
     "Is the founding-problem appeal genuine principle or cover story -> 'examine whether courts "
     "apply founding-problem logic consistently' across powerful/powerless speech — external "
     "observation of jurisprudential practice. measure external -> Ω_E. Authored conceptual; the "
     "resolution is empirical (consistency-of-application is observed). MISTYPED -> Ω_E."),
 19: (["define"], "criterion", "Ω_C", "medium",
     "FAMILY. 'Which reading of the climate kernel is structurally defensible?' Approach lists "
     "empirical falsification AND institutional lock-in, but the core resolution is the kernel "
     "contest itself — a framework-selection over the declared triad (mitigation/adaptation/"
     "degrowth). The deciding move is conceptual/framework. define external -> Ω_C. The measure "
     "limb is one input not the locus. Authored conceptual: agree."),
 20: (["define"], "criterion", "Ω_C", "medium",
     "FAMILY. Does a homoiousios reading produce a structurally different constraint than "
     "homoousios? 'Generate a parallel constraint story ... compare base properties.' The deciding "
     "locus is framework selection between two doctrinal readings (the alternative reading is the "
     "open term); the 'compare' is over a GENERATED sibling, not an unobserved external fact. "
     "define external -> Ω_C. Authored conceptual: agree. (Baseline lexically mis-fired measure.)"),
 21: (["define", "measure"], "observation,criterion", "hybrid(define+measure)", "medium",
     "Does the 'first institutional holding' boundary correctly mark electronic-money emergence, "
     "vs 'became thinkable' / 'M4-M5' readings? Two operations genuinely fire: (define) which "
     "boundary CRITERION counts as emergence — an open conceptual choice among declared sibling "
     "readings; AND (measure) 'trace institutional decisions, regulatory filings, economic "
     "outcomes — did behavior change when...' — an external observation of when behavior actually "
     "changed. Both external -> genuine hybrid(define+measure). Authored empirical: the measure "
     "limb is external so authored-Ω_E IS among the fired externals -> hybrid counts as AGREE."),
 22: (["measure"], "observation", "Ω_E", "high",
     "Was U.S. exorbitant privilege structurally necessary or a constrainable side effect -> "
     "'comparison with alternative arrangements; historical counterfactuals from economists "
     "(Steil, Eichengreen) comparing actual growth outcomes'. External historical/counterfactual "
     "observation. measure external -> Ω_E. Authored conceptual. MISTYPED -> Ω_E."),
 23: (["measure"], "observation", "Ω_E", "high",
     "Would Tesla's innovation improve/decline under independent governance -> 'counterfactual "
     "scenario modeling vs peers; natural experiment if divestiture occurs; historical analysis "
     "of Musk time allocation'. External empirical counterfactual. measure external -> Ω_E. "
     "Authored conceptual. MISTYPED -> Ω_E."),
 24: (["define"], "criterion", "Ω_C", "high",
     "FAMILY. Does liturgical-continuity reading foreclose native-generation reading? Resolution "
     "explicitly: 'clarification of what \"living\" means in each reading's framework' — a textbook "
     "define at criterion-locus; the term 'living' is the open criterion the declared readings "
     "leave unspecified. define external -> Ω_C. Authored conceptual: agree."),
 25: (["define"], "criterion", "Ω_C", "high",
     "FAMILY. Do the three sibling readings partition the conceptual space exhaustively, or do "
     "missed readings exist? 'Genealogical analysis ... are there framings that cannot be mapped "
     "to one of the three?' This is a conceptual completeness/definition call over the declared "
     "reading set. define external -> Ω_C. Authored conceptual: agree."),
 26: (["measure"], "observation", "Ω_E", "high",
     "Does bloodline succession resolve disputes or generate them -> 'empirical count of "
     "succession crises per century under monarchical/elective/designated regimes'. External "
     "observation. measure external -> Ω_E. Authored empirical: agree. (Baseline's decide misfired "
     "on 'should compute toward snare' — not a value question.)"),
 27: (["define"], "criterion", "Ω_C", "medium",
     "FAMILY. Does the commemorative-husk reading instantiate a different constraint than the "
     "behavioral-competence reading, or same object at different epistemic positions? Resolution "
     "is 'the ε-invariance test'. KEY: ε-invariance here is NOT re-deriving an authored ε — the "
     "two readings' ε are not both declared; the test DEFINES whether two constraints exist by a "
     "framework criterion (same-object-vs-distinct). The operative locus is conceptual: criterion "
     "(constraint individuation) is the open term. define external -> Ω_C. Authored conceptual: "
     "agree. (Baseline returned unknown — fail-closed on the decide+define+measure triple.)"),
 28: (["define"], "criterion", "Ω_C", "high",
     "FAMILY. Does choosing the messianic_suspension reading depend on the reader's committer "
     "frame rather than constraint-structure? 'Textual history and community survey: trace which "
     "institutional positions hold which reading.' Surface looks measure (survey), but the DECIDING "
     "question is conceptual: whether reading-choice is frame-dependent (a definition of what makes "
     "readings 'genuinely alternative' vs incommensurable). The survey is evidence FOR the "
     "conceptual call. define external -> Ω_C; measure is subordinate. Authored conceptual: agree."),
 29: (["measure"], "observation", "Ω_E", "high",
     "Has the founding problem (preventing ideological intervention) been solved or replaced -> "
     "'historical assessment of intervention motives over time; counterfactual analysis'. External "
     "historical observation. measure external -> Ω_E. Authored conceptual. MISTYPED -> Ω_E."),
 30: (["define"], "criterion", "Ω_C", "high",
     "Does the Martens Clause bind IHL content to fixed interpretations or permit evolution? "
     "'Textual and jurisprudential analysis of invocations.' The open criterion is the meaning/"
     "scope of 'principles of humanity / public conscience' — a definitional/framework call. "
     "define external -> Ω_C. Authored conceptual: agree."),
 31: (["define"], "criterion", "Ω_C", "high",
     "FAMILY. Is this reading's distinction from siblings stable/structural, or do the three "
     "collapse into one constraint with three interpretations? 'ε-invariance test': do the readings "
     "produce different victim/beneficiary sets? Again the ε-invariance is a CONSTRAINT-INDIVIDUATION "
     "criterion (define), not an authored-ε re-derivation — the comparison is over whether distinct "
     "constraints exist. define external -> Ω_C. Authored conceptual: agree."),
 32: (["define"], "criterion", "Ω_C", "high",
     "Does revisable_translation reading entail scholarship-revision is the ONLY legitimate "
     "approach, or permit other frames? 'Examine how defenders respond to rival readings: claim "
     "incoherence or mere superiority?' The open criterion is exclusivity-vs-pluralism of the "
     "reading — a conceptual/definitional determination over the declared readings. The 'legitimate' "
     "wording is about reading-relation, not a stakeholder value-decision. define external -> Ω_C. "
     "Authored conceptual: agree."),
 33: (["decide", "define"], "decider", "Ω_P", "high",
     "PREFERENCE. 'Is Global South development legitimately constrained by planetary boundaries, "
     "or is this an illegitimate barrier encoding Global North privilege?' The deciding locus is a "
     "value/equity judgment about whose burden-sharing allocation is legitimate — a stakeholder "
     "value not settled by declared fields (the approach lists rival ALLOCATION FRAMEWORKS = "
     "enumerated options, but the decider that picks among equal/historical/needs allocation is "
     "external). Enumerated options != restatement. decide external (decider) -> Ω_P. Agree."),
 34: (["measure"], "observation", "Ω_E", "high",
     "Is suppression (0.12) structural or internalized -> 'post-removal counterfactual: if race-"
     "conscious policy were permitted, would actors adopt it or persist in colorblindness?' "
     "External observed counterfactual. measure external -> Ω_E. Authored empirical: agree."),
 35: (["define"], "criterion", "Ω_C", "medium",
     "Does the living reading contain an internal principle limiting recognized rights, or admit "
     "indefinite expansion? 'Jurisprudential analysis: is there a stated criterion (deeply-rooted, "
     "ordered-liberty, dignity)?' The deciding locus is whether a LIMITING CRITERION exists/what it "
     "is — a definitional question. 'How often are claims rejected' is evidence but the locus is the "
     "criterion. define external -> Ω_C. Authored empirical. MISTYPED -> Ω_C "
     "(an Ω_E-falsifier: authored-Ω_E whose resolution routes via define, not measure)."),
 36: (["measure"], "observation", "Ω_E", "high",
     "Do practitioners constrain practice from theological acceptance or from fear/internalization "
     "-> 'post-prohibition ethnography: when enforcement lapses, do they resume practice?' External "
     "observed counterfactual. measure external -> Ω_E. Authored empirical: agree."),
 37: (["measure"], "observation", "Ω_E", "high",
     "Do jihadist movements genuinely believe the abrogating reading or strategically deploy it -> "
     "'internal documentary evidence (recruitment materials, treatises, leadership statements)'. "
     "External observation of belief/intent. measure external -> Ω_E. Authored empirical: agree."),
 38: (["define"], "criterion", "Ω_C", "medium",
     "FAMILY. Is the Hanafi reading coexistent with Hanbali/Maliki/Shafi'i, or does Hanafi "
     "ascendance foreclose them at the framework level? 'Examination of whether rival texts remain "
     "available/taught/invoked.' Surface looks measure (availability), but the DECIDING question is "
     "the foreclose-vs-coexist RELATION among declared readings — a conceptual criterion. The "
     "availability evidence is the operationalization. define external -> Ω_C; measure subordinate. "
     "Authored conceptual: agree."),
 39: (["define"], "criterion", "Ω_C", "medium",
     "PREFERENCE (authored). 'Does priority of non-Western conceptualization constitute evidence "
     "of cultural contingency, or is it merely a historical fact?' Resolution is 'philosophical "
     "analysis of what \"priority\" means' — an open definitional criterion (does earlier "
     "formalization imply dependency or only precedence?). define external -> Ω_C. Authored "
     "preference; but the locus is conceptual not a value-decision: 'who decides' does not settle "
     "it — what 'priority' MEANS settles it. MISTYPED -> Ω_C (an authored-Ω_P that routes define)."),
}

EXP = {"Ω_C": "conceptual", "Ω_E": "empirical", "Ω_P": "preference"}

def vs_authored(diag, externals_list, authored):
    """Hybrid-aware: hybrid agrees if authored type is among fired externals."""
    if diag.startswith("hybrid"):
        inside = diag[diag.index("(")+1:diag.index(")")].split("+")
        sig_type = {"define": "conceptual", "decide": "preference", "measure": "empirical"}
        ext_types = {sig_type[s] for s in inside}
        if authored in ext_types:
            return "agree"
        return f"mistyped->{diag}"
    if diag == "restatement":
        return "restatement-mislabel"
    pure = EXP.get(diag)
    if pure == authored:
        return "agree"
    return f"mistyped->{diag}"

def main():
    data = {r["sample_id"]: r for r in json.load(open(SAMPLE, encoding="utf-8"))}
    out = []
    for sid in sorted(V):
        fired, external_to, diag, conf, rationale = V[sid]
        r = data[sid]
        authored = r["omega_type"]
        out.append({
            "sample_id": sid, "name": r["name"], "authored_type": authored,
            "is_family": r["is_family"],
            "fired_signatures": fired, "external_to": external_to,
            "diagnosis": diag,
            "vs_authored": vs_authored(diag, external_to, authored),
            "confidence": conf, "rationale": rationale,
        })
    json.dump(out, open(OUT, "w"), indent=2, ensure_ascii=False)
    # summary
    agree = sum(1 for o in out if o["vs_authored"] == "agree")
    print(f"n={len(out)}  agree(hybrid-aware)={agree}  rate={agree/len(out):.3f}")
    print("vs_authored:", dict(collections.Counter(o["vs_authored"] for o in out)))
    print("diagnosis  :", dict(collections.Counter(
        ("hybrid" if o["diagnosis"].startswith("hybrid") else o["diagnosis"]) for o in out)))
    print(f"wrote {OUT}")

if __name__ == "__main__":
    main()
