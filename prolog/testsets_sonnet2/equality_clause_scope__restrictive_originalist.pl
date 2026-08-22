% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive-Originalist Reading of the Equality Clause (Founding-Era Franchise Scope)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This story authors ONE reading — the restrictive-originalist reading — of
 *   the contested equality_clause_scope kernel. On this reading, the founding
 *   text's equality guarantee is scoped by the 18th-century social-contract
 *   theory that produced it: political actorhood attaches to propertied white
 *   men, and any extension beyond that class requires a new constitutional
 *   act (amendment), not judicial reinterpretation of the original clause.
 *   The reading's own metrics describe the original arrangement AS THIS
 *   READING SEES IT — a genuine coordination function (settling who could
 *   consent to and participate in governance) riding on top of severe,
 *   actively-enforced exclusion of enslaved persons, free Black citizens,
 *   women, and landless men. The sibling readings (expansive_universalist,
 *   progressive_textualist) are separate constraint files with their own ε
 *   and their own stakeholder sets; this file does not average over them or
 *   hedge ε across them.
 *
 * KEY AGENTS:
 *   - propertied_white_male_citizens: primary beneficiary — full political actor status
 *   - founding_era_political_class: agenda_setter — fixed the original scope
 *   - originalist_judicial_interpreters: agenda_setter/beneficiary — enforces the restrictive boundary today
 *   - enslaved_persons, free_black_citizens, women_of_all_classes, landless_white_men: payers — excluded from the political-actor class by original scope
 *   - constitutional_historians: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.68).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.79).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.68).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive-Originalist Reading of the Equality Clause (Founding-Era Franchise Scope)").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '7c8add32-cf84-4b97-87b3-bdb7693532a4').
narrative_ontology:cs_kernel_codification('7c8add32-cf84-4b97-87b3-bdb7693532a4', fixed_text).
narrative_ontology:cs_authority_grounding('7c8add32-cf84-4b97-87b3-bdb7693532a4', lineage).
narrative_ontology:cs_interpretation_layer_present('7c8add32-cf84-4b97-87b3-bdb7693532a4').
narrative_ontology:cs_reading_relation('7c8add32-cf84-4b97-87b3-bdb7693532a4', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('7c8add32-cf84-4b97-87b3-bdb7693532a4', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('7c8add32-cf84-4b97-87b3-bdb7693532a4', foundational, political_personhood_scoped_by_founding_social_contract).
narrative_ontology:cs_axiom_status(political_personhood_scoped_by_founding_social_contract, holdable).
narrative_ontology:cs_axiom_grounding('7c8add32-cf84-4b97-87b3-bdb7693532a4', political_personhood_scoped_by_founding_social_contract, conventional).
narrative_ontology:cs_axiom('7c8add32-cf84-4b97-87b3-bdb7693532a4', foundational, scope_expansion_requires_formal_amendment_not_judicial_discovery).
narrative_ontology:cs_axiom_status(scope_expansion_requires_formal_amendment_not_judicial_discovery, holdable).
narrative_ontology:cs_axiom_grounding('7c8add32-cf84-4b97-87b3-bdb7693532a4', scope_expansion_requires_formal_amendment_not_judicial_discovery, conventional).
narrative_ontology:cs_reference_frame('7c8add32-cf84-4b97-87b3-bdb7693532a4', founding_era_social_contract_membership).
narrative_ontology:cs_drift_state('7c8add32-cf84-4b97-87b3-bdb7693532a4', post_reconstruction_and_suffrage_amendments, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7c8add32-cf84-4b97-87b3-bdb7693532a4', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, founding_era_political_class).
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, originalist_judicial_interpreters).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_black_citizens).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women_of_all_classes).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, landless_white_men).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, social_contract_membership_theory).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the franchise, can hold office, own property, and enter binding contracts as full political actors under the founding-era arrangement. The equality clause, on this reading, secures their formal equality with one another (no titles of nobility, no crown-derived privilege among themselves) without extending further. They benefit from a stable, textually anchored definition of who counts as a rights-bearing political actor.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, beneficiary,
    powerful, generational, arbitrage, national).

% Drafted the founding text against a specific social-contract theory in which political personhood was tied to property, race, and sex. They set the original scope and, on this reading, that scope is the fixed reference point against which all later claims must be measured against amendment, not reinterpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, founding_era_political_class, agenda_setter,
    institutional, civilizational, analytical, national).

% Enforce the restrictive reading by requiring any expansion of equality's application to rest on subsequent amendment rather than judicial discovery of an implicit universal principle. They gain interpretive authority and doctrinal stability from anchoring meaning to founding-era social facts, and their professional legitimacy is bound to defending that anchor.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_judicial_interpreters, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__restrictive_originalist, originalist_judicial_interpreters, beneficiary).

% Categorically excluded from the political-actor class the clause protects; the founding-era social contract treats them as property, not party. Under this reading, the equality clause offers no textual purchase for their claims at all — the exclusion is by design, not oversight, and it requires their emancipation and separate constitutional amendment to even become a rights question.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_persons, payer,
    powerless, biographical, trapped, national).

% Even where nominally free, they fall outside the racial boundary of the original social-contract membership on this reading. They bear the costs of exclusion from the franchise and from full contractual and political standing, with no textual basis in the original clause to contest it — their remedy runs only through the amendment process, decades away.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_black_citizens, payer,
    powerless, biographical, trapped, national).

% Excluded from political-actor status regardless of property or race, since the founding-era social contract defines the political subject as male. On this reading their exclusion is not an interpretive gap to be closed by courts but a scope boundary that persists until a specific amendment addresses it directly.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women_of_all_classes, payer,
    powerless, biographical, trapped, national).

% Nominally within the racial and sex boundary but excluded by the property qualification embedded in the original social-contract theory. Their path into the political-actor class runs through acquiring property or through state-level franchise reform, not through a reinterpretation of the equality clause itself.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, landless_white_men, payer,
    powerless, biographical, constrained, national).

% Argue the equality principle was always meant as a self-evident universal truth that founding-era practice simply failed to honor. This reading treats their position as importing a claim the text does not textually support at the moment of ratification; their advocacy is directed at amendment and later doctrine, not at this reading's account of original scope.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansive_universalist_advocates, excluded,
    organized, generational, constrained, national).

% Study the drafting record, ratification debates, and contemporaneous social-contract theory to establish what the founding generation understood themselves to be doing. Their findings are cited by all three kernel readings but adjudicate none of them outright.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, diffuse).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, textually bounded definition of who counts as a political actor under the founding social contract, allowing the founding generation to coordinate governance among themselves without ambiguity about who held standing to consent, vote, hold office, and contract.
% TRANSFER_FUNCTION: Moves political standing, franchise access, and contractual capacity to the bounded class of propertied white men, while withholding all three from enslaved persons, free Black citizens, women, and landless men — treating the withholding as scope rather than injury under this reading's own terms.
% ABSENT_VOICES: Enslaved persons, free Black citizens, women, and landless men had no seat in the drafting or ratification process that fixed this scope; their objections were not absent from history but were absent from the room where the boundary was set, and this reading treats that absence as constitutionally settled rather than as an open wound requiring judicial repair.
% DISAPPEARANCE_RATIONALE: If this restrictive-originalist reading were abandoned as the operative interpretive frame, courts would be free to treat the equality principle as reaching further than the amendment record specifies, collapsing the distinction this reading insists on between textual scope and moral aspiration — legitimacy questions about judicial versus democratic expansion of rights would have to be resolved on entirely different grounds.
% FOUNDING_PROBLEM: The founding generation needed a way to establish who held political standing to consent to government, vote, hold office, and enter binding contracts, drawing on a social-contract theory that defined the political subject as propertied, white, and male.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the ratification debates and constitutional scholars outside the originalist interpretive tradition (including drafters of the Reconstruction and suffrage amendments themselves, who treated the original scope as requiring formal amendment rather than reinterpretation) attest that the founding-era problem of defining political membership along these lines is no longer live as a matter of positive law — the amendments themselves are the outside corroboration that the original scope was understood by later political actors as requiring correction, not merely extension.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts high (0.81) at the founding moment because the exclusion was total and enforced by law (slave codes, coverture, property qualifications) and declines slowly over the 240-year interval as amendments (13th, 14th, 15th, 19th, 24th, 26th) and enforcement statutes carve out the excluded groups one by one — but on THIS reading, that decline is due to separate constitutional acts layered onto the original clause, not a reinterpretation of the original clause's scope, so residual extractiveness (0.68) remains high because the reading treats the original clause itself as still narrowly scoped even after the amendments changed the surrounding law. Suppression starts near-maximal (0.95, slave codes and coverture enforcement) and eases as amendments remove the legal machinery of exclusion, though it ticks back up modestly at the end (0.79) reflecting renewed disputes over whether the amendment-based expansions are themselves under interpretive threat. Theater ratio rises over time (0.10 to 0.42) as originalist doctrine increasingly performs textual fidelity to justify a scope that grows harder to defend once nearly universal suffrage and civil rights law surround it.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (propertied white men, originalist interpreters), the original scope is a coherent, textually faithful account of what the founders actually committed to — a genuine coordination achievement given 18th-century political theory. From the payer seats, the same textual fidelity operates as an enforced exclusion that required generations of political struggle and formal amendment to dislodge, precisely because the reading refuses to let judicial interpretation do that work. The engine's per-seat computation should reflect this divergence structurally rather than through any authored hedge.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white male citizens and the founding-era political class sit at the beneficiary end: the clause, on this reading, was drafted to secure and formalize their political standing, and they hold arbitrage-grade exit (they can invoke, ignore, or reinterpret the clause as suits their interests within the political system they control). Originalist judicial interpreters share beneficiary status because their doctrinal authority depends on defending this scope. The four payer groups are trapped or constrained: enslaved persons and free Black citizens have no textual purchase within the clause itself on this reading (trapped, requiring emancipation and later amendment); women have no purchase regardless of race or property (trapped until the 19th Amendment); landless white men are constrained rather than trapped because a path into the beneficiary class (acquiring property, later suffrage reform) exists without a new constitutional act.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — defining who holds standing to consent to government under a specific social-contract theory — is dead as live law (superseded by subsequent amendments establishing broader personhood and franchise), but the restrictive-originalist READING of the original clause persists as an interpretive commitment used to police the boundary between judicial expansion and democratic amendment. This is not itself mandatrophy of the constraint (amendments did fix the underlying problem for the excluded groups) but the reading's insistence that the ORIGINAL clause remains narrowly scoped notwithstanding those amendments is the site of contest with the sibling readings — a mismatch the disappearance_verdict and founding_problem_status fields are designed to surface, not adjudicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_scope_vs_universal_principle_ambiguity,
    'Did the founding generation draft the equality clause intending a universal principle they hypocritically failed to apply, or did they draft a genuinely scope-limited principle consistent with their own social-contract theory?',
    'Close historical analysis of ratification debates, contemporaneous political philosophy texts cited by the drafters, and drafting-history records distinguishing aspirational language from operative legal scope.',
    'If the founders intended a universal principle, the restrictive-originalist reading is itself a later interpretive imposition and the expansive_universalist reading''s core premise is closer to the historical fact; if they intended a scope-limited principle, this reading''s account of original meaning stands and the burden shifts entirely to the amendment process for any expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_scope_vs_universal_principle_ambiguity, conceptual, 'Whether the founding-era text encoded a universal or a scope-limited equality principle — the central interpretive fork between this reading and its expansive_universalist sibling.').

omega_variable(
    amendment_as_correction_vs_extension,
    'Do the Reconstruction and suffrage amendments CORRECT a founding error (implying the original clause was always meant more broadly) or EXTEND a deliberately narrow original scope to new classes (implying the restrictive reading was correct about the original text)?',
    'Analysis of the amendments'' own drafting history and the rhetoric used to justify them — did framers of the 14th and 19th Amendments describe themselves as fulfilling founding intent or as overriding it?',
    'This directly locates the disagreement with the progressive_textualist sibling reading, which shares this reading''s amendment-centric mechanism but may treat the amendments as correcting an original wrong rather than confirming original narrowness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_as_correction_vs_extension, conceptual, 'Whether amendment-based expansion confirms or repudiates the restrictive reading''s account of original scope.').

omega_variable(
    false_summit_naturalization_risk,
    'Does presenting the founding-era social-contract scope as the historically accurate ''original meaning'' risk naturalizing what was in fact a constructed political settlement that benefited a specific propertied racial and sex class?',
    'Cross-check against comparative constitutional history: did other founding-era republics with different property/race/sex qualifications produce equally ''natural'' readings of their own equality provisions, suggesting scope reflects contingent political bargaining rather than an inevitable social-contract logic?',
    'If scope reflects contingent bargaining rather than an inevitable logic, the restrictive-originalist reading functions partly as a legitimating story for the founding beneficiary class rather than a neutral historical description — this bears directly on whether tangled_rope (coordination-with-extraction) rather than a more purely descriptive classification is the correct structural read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_risk, empirical, 'Whether the originalist scope claim is historically descriptive or partly a legitimating construction benefiting the founding-era political class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__restrictive_originalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__restrictive_originalist, theater_ratio, 40, 0.15).
narrative_ontology:measurement(equa_tr_t80, equality_clause_scope__restrictive_originalist, theater_ratio, 80, 0.22).
narrative_ontology:measurement(equa_tr_t120, equality_clause_scope__restrictive_originalist, theater_ratio, 120, 0.3).
narrative_ontology:measurement(equa_tr_t160, equality_clause_scope__restrictive_originalist, theater_ratio, 160, 0.36).
narrative_ontology:measurement(equa_tr_t200, equality_clause_scope__restrictive_originalist, theater_ratio, 200, 0.4).
narrative_ontology:measurement(equa_tr_t240, equality_clause_scope__restrictive_originalist, theater_ratio, 240, 0.42).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__restrictive_originalist, base_extractiveness, 0, 0.81).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__restrictive_originalist, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(equa_be_t80, equality_clause_scope__restrictive_originalist, base_extractiveness, 80, 0.75).
narrative_ontology:measurement(equa_be_t120, equality_clause_scope__restrictive_originalist, base_extractiveness, 120, 0.71).
narrative_ontology:measurement(equa_be_t160, equality_clause_scope__restrictive_originalist, base_extractiveness, 160, 0.7).
narrative_ontology:measurement(equa_be_t200, equality_clause_scope__restrictive_originalist, base_extractiveness, 200, 0.69).
narrative_ontology:measurement(equa_be_t240, equality_clause_scope__restrictive_originalist, base_extractiveness, 240, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__restrictive_originalist, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__restrictive_originalist, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(equa_su_t80, equality_clause_scope__restrictive_originalist, suppression_requirement, 80, 0.86).
narrative_ontology:measurement(equa_su_t120, equality_clause_scope__restrictive_originalist, suppression_requirement, 120, 0.78).
narrative_ontology:measurement(equa_su_t160, equality_clause_scope__restrictive_originalist, suppression_requirement, 160, 0.7).
narrative_ontology:measurement(equa_su_t200, equality_clause_scope__restrictive_originalist, suppression_requirement, 200, 0.75).
narrative_ontology:measurement(equa_su_t240, equality_clause_scope__restrictive_originalist, suppression_requirement, 240, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__restrictive_originalist, 0.1).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the equality_clause_scope kernel, each a structurally distinct constraint with its own ε and stakeholder set per the ε-invariance principle. restrictive_originalist authors the narrowest beneficiary set and treats amendment as the exclusive legitimate mechanism for scope expansion. expansive_universalist authors near-zero founding-era legitimate extraction (treating exclusion as failure to honor an always-universal principle). progressive_textualist sits closer to this reading on mechanism (amendment over reinterpretation) but differs on whether original scope was ever meant to be narrow. All three link to each other via affects_constraints; do not average or reconcile their ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
