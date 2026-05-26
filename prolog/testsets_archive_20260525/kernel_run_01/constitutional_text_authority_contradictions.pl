% Axiom contradictions for kernel: constitutional_text_authority
% Source: SCOPE axiom_contradictions declaration (independent of edge types).
% contradiction + coexists_with edge = licensed plurality
% contradiction + forecloses edge    = real closure

:- multifile narrative_ontology:cs_axiom_contradiction/2.

% originalist_reading↔living_constitutionalist_reading: Originalism holds constitutional meaning is fixed at ratification and cannot change without Article V amendment; living constitutionalism holds meaning evolves with social values even without formal amendment. No single coherent framework can hold both that Brown v. Board (1954) changed the Constitution's meaning (living) and that it merely discovered the 14th Amendment's original 1868 meaning (originalist). The axioms are mutually exclusive on whether judicial interpretation can alter constitutional constraints.
% positivist_reading↔living_constitutionalist_reading: Positivism holds law's validity is independent of moral content and derives solely from formal enactment; living constitutionalism holds constitutional meaning must evolve to reflect contemporary moral values and principles. No single framework can hold both that moral principles are irrelevant to constitutional validity (positivist) and that evolving moral values change constitutional meaning (living). The law/morality separation axiom contradicts the moral-evolution axiom.

narrative_ontology:cs_axiom_contradiction(meaning_fixed_at_ratification, constitutional_meaning_evolves_with_moral_understanding).
narrative_ontology:cs_axiom_contradiction(constitutional_meaning_evolves_with_moral_understanding, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_contradiction(law_morality_distinction_exhaustive, constitutional_meaning_evolves_with_moral_understanding).
narrative_ontology:cs_axiom_contradiction(constitutional_meaning_evolves_with_moral_understanding, law_morality_distinction_exhaustive).
