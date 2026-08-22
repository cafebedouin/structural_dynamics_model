% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Priestly Polytheistic Legitimacy (Amun-Ra Cosmology)
 *   domain: political_economy/religious_systems/ancient_governance
 *
 * SUMMARY:
 *   This constraint models divine legitimacy flowing through priestly
 *   interpretation of a multi-deity cosmology centered on Amun-Ra in ancient
 *   Egyptian governance. The pharaoh requires priestly validation to rule;
 *   priests interpret divine will through cosmological doctrine; temple
 *   economies accumulate wealth justified as sacred service. Regional
 *   non-conforming cults and alternative theological readings (Atenism, folk
 *   syncretism) are suppressed or subordinated. This reading instantiates the
 *   polytheistic, distributed-authority interpretation of the contested
 *   divine legitimacy kernel. The constraint is CLAIMED as tangled_rope
 *   (genuine coordination function enabling pharaonic succession, paired with
 *   asymmetric extraction benefiting temples and priestly class) while the
 *   authored measurements show increasing extractiveness and growing theater
 *   ratio over the interval — a pattern consistent with institutional capture
 *   of the coordination function as temple economies consolidate wealth.
 *
 * KEY AGENTS:
 *   - Priestly class: interpreters of cosmology, controllers of ritual performance, beneficiaries of land/labor accumulation (organized, generational horizon, identity-locked exit)
 *   - Temple economies: institutional accumulator of wealth justified by the constraint's operation (institutional power, generational horizon, mobile exit at institutional level)
 *   - Pharaoh: requires priestly legitimacy, constrained by cosmological doctrine, structurally dependent (powerful but constrained, biographical horizon, trapped exit from the constraint)
 *   - Regional non-conforming cults: suppressed alternative practices, forced subordination to Amun-Ra hierarchy (moderate power, biographical horizon, constrained exit)
 *   - Common subjects: beneficiaries of cosmological coherence and ritual structure, powerless to exit (powerless, biographical horizon, trapped exit)
 *   - Rival legitimacy factions (Atenists, syncretists): excluded from priestly consensus, structurally suppressed (powerful to moderate power per faction, constrained exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.71).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Priestly Polytheistic Legitimacy (Amun-Ra Cosmology)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "political_economy/religious_systems/ancient_governance").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '8ab67d49-8ede-4124-9134-99edcf469c88').
narrative_ontology:cs_kernel_codification('8ab67d49-8ede-4124-9134-99edcf469c88', fixed_text).
narrative_ontology:cs_authority_grounding('8ab67d49-8ede-4124-9134-99edcf469c88', lineage).
narrative_ontology:cs_interpretation_layer_present('8ab67d49-8ede-4124-9134-99edcf469c88').
narrative_ontology:cs_reading_relation('8ab67d49-8ede-4124-9134-99edcf469c88', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ab67d49-8ede-4124-9134-99edcf469c88', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('8ab67d49-8ede-4124-9134-99edcf469c88', foundational, pantheon_hierarchy_epistemically_necessary).
narrative_ontology:cs_axiom_status(pantheon_hierarchy_epistemically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('8ab67d49-8ede-4124-9134-99edcf469c88', pantheon_hierarchy_epistemically_necessary, deontological).
narrative_ontology:cs_axiom('8ab67d49-8ede-4124-9134-99edcf469c88', foundational, priestly_interpretation_authority_legitimacy).
narrative_ontology:cs_axiom_status(priestly_interpretation_authority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8ab67d49-8ede-4124-9134-99edcf469c88', priestly_interpretation_authority_legitimacy, conventional).
narrative_ontology:cs_reference_frame('8ab67d49-8ede-4124-9134-99edcf469c88', amun_supremacy_priestly_mediation_model).
narrative_ontology:cs_drift_state('8ab67d49-8ede-4124-9134-99edcf469c88', mid_to_late_new_kingdom_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ab67d49-8ede-4124-9134-99edcf469c88', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, priestly_class).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_power_autonomy).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, regional_non_conforming_cults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, common_subjects).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, amun_supremacy_in_pantheon).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, priestly_interpretation_authority).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, cosmological_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the multi-deity cosmology, validates pharaonic rule through priestly performance and textual exegesis, maintains the ritual calendar and theological coherence of the pantheon. Their authority derives from succession within priestly families and textual control. They collect wealth through temple lands, offerings, and labor-service obligations. Their exit from this role is impossible: the priesthood is constituted through initiation lineage and sacred knowledge; a priest ceasing to perform the role ceases to be a priest.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, priestly_class, agenda_setter,
    organized, generational, identity_locked, national).

% Vast temple complexes accumulate land, labor, and offerings under the constraint's operation. Temple economies operate semi-autonomously from pharaonic administration in many cases. They grow wealthier as the legitimacy constraint demands more elaborate rituals, festivals, and monumental building. The constraint validates their wealth accumulation as sacred rather than extractive.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies, beneficiary,
    institutional, generational, mobile, national).

% Requires priestly validation to govern legitimately; cannot rule by force alone within the cosmological framework this constraint instantiates. Must perform costly rituals, subsidize temple operations, and defer to priestly interpretation of divine will. In return, the priestly class extends legitimacy to the pharaonic office and constrains ambitious priests from directly seizing power. The pharaoh is structurally constrained: exit from the constraint means losing divine legitimacy and facing succession challenge or priestly opposition.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary).

% Local deities and cult practices that do not fit neatly into the Amun-Ra centered cosmology must either accommodate themselves to the official framework or face suppression as heretical. They pay by surrendering autonomy and subordinating local deities to Amun-Ra's hierarchy. Their priesthoods lose status and wealth as the central temple system consolidates authority. They retain some ritual autonomy but within bounds the priestly class monitors.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_non_conforming_cults, payer,
    moderate, biographical, constrained, regional).

% Receive the constraint's coordination functions: a coherent, unified cosmology that makes sense of the world, predictable ritual calendar, institutional channels for petition to the divine through priestly mediation. They participate as audience and labor-contributor to temple operations. Their religious needs are addressed by the priestly class; they have no alternative structure for legitimating authority or making sense of cosmological order.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, common_subjects, beneficiary,
    powerless, biographical, trapped, national).

% Atenist reformers and folk-syncretistic practitioners are structurally excluded from the priestly consensus that this constraint represents. They would challenge the Amun-Ra supremacy and the priestly interpretive monopoly if given platform; the constraint's enforcement keeps their alternative readings subordinate or clandestine. Their exclusion is what the constraint's active enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, rival_legitimacy_factions, excluded,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, rival_legitimacy_factions, observer).

% Records and reports on temple operations, ritual observance, and pharaonic legitimacy claims. Sits outside the priestly succession but depends on the constraint for its interpretive authority (what counts as a legitimate written record). Records both the official cosmology and occasional alternatives without endorsing either — witnesses to the constraint without power to change it.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, scribal_administrative_class, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economies).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified cosmological framework that makes pharaonic rule intelligible and authoritative: the pharaoh mediates between the divine realm (where Amun-Ra presides over the pantheon) and the human realm. Priests interpret divine will and coordinate the ritual calendar that maintains cosmic order. This solves the problem of succession legitimacy and administrative coherence across a vast territory with diverse local cults.
% TRANSFER_FUNCTION: Moves wealth (land, labor, offerings) from the broader economy to the temple institutions; moves authority to interpret divine will from local cults and common practitioners to the priestly class; moves the pharaoh's autonomy toward dependence on priestly validation. In return, the priestly class extends legitimacy to pharaonic rule and coordinates a stable interpretive framework.
% ABSENT_VOICES: Atenist monotheistic reformers (who would argue Aten alone is divine and the priesthood's multi-deity cosmology is false idolatry); folk-syncretistic practitioners (who would argue local deities retain autonomy and priestly interpretation over-constrains legitimate religious practice); women's spiritual authority outside priestly structures; merchant and artisan classes who might organize alternative legitimacy claims around craft-guild or economic authority rather than cosmological position.
% DISAPPEARANCE_RATIONALE: If the constraint vanished — if priestly interpretation of Amun-Ra cosmology ceased to validate pharaonic rule — the pharaonic succession mechanism would face immediate crisis (alternative claimants would lack religious legitimacy); temple economies would lose justification for their wealth accumulation; regional cults would reassert local deity autonomy; Atenist reformers or syncretistic practitioners would have opening to restructure legitimacy around different cosmological frames. The entire governance apparatus depended on this constraint's operation.
% FOUNDING_PROBLEM: Early dynastic Egypt required a mechanism to coordinate rule across territorial scale and ethnic diversity; a mechanism to transition power through succession without civil war; a mechanism to integrate heterogeneous regional cults into a single administrative order; and a mechanism to explain why the pharaoh's commands should be obeyed by those who never see the pharaoh's face. The priestly-cosmological framework solved these by grounding pharaonic authority in divine appointment mediated through priestly interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Temple inscriptions and pharaonic declarations attest the founding problem remains live — cosmic disorder threatens if pharaonic rule lapses or legitimacy fails. Scribal records document the constraint's operation and enforcement. Atenist critics (Akhenaten's reign records, later priestly condemnations of heresy) attest the problem is substantially solved and the constraint persists as priestly wealth consolidation; folk practitioners document pragmatic religion operating without priestly mediation, suggesting legitimacy and order can be maintained without the constraint. The corpus of temple inscriptions, pharaonic decrees, and external observations from neighboring societies provides corroboration independent of the benefiting priestly class alone.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate-high (0.62 at interval end) because the constraint moves substantial wealth to temples without proportional service cost and because pharaonic autonomy is traded for priestly validation. Suppression is high (0.71) because the constraint's persistence depends on actively preventing Atenist and syncretistic alternatives from gaining legitimacy — the enforcement machinery targets rival theological readings as heretical. Theater ratio rises over the interval (0.28 to 0.45) suggesting that as temple economies consolidate wealth, an increasing share of priestly activity is performative wealth-defense rather than genuine cosmological function — the constraint's founding coordination problem (explaining pharaonic succession) remains solved, but the extractive rental component grows. Accessibility collapse is high (0.68) because once the priestly-cosmological framework is internalized, alternatives seem cosmically illegitimate rather than merely organizationally suppressed — subjects cannot imagine rule without priestly mediation. Resistance is moderate (0.52) reflecting periodic reform attempts (Akhenaten's Atenism, folk-cult persistence) that the constraint must actively suppress.
 *
 * PERSPECTIVAL GAP:
 *   The priestly reading of the constraint emphasizes coordination (cosmic order maintained through proper ritual, pharaonic legitimacy ensured through divine appointment) and frames extraction as the costs of that coordination (temples require wealth to perform rituals, priestly training requires resources). The payer seats (pharaoh, regional cults, subjects) experience substantial extraction: wealth moved without matching service cost, theological autonomy lost, risk of suppression if they attempt exit. The engine computes this divergence from the structural data: beneficiary/payer asymmetry + active enforcement + suppression of alternatives = tangled_rope classification even though the priestly framing is pure coordination. The theater ratio rise indicates the coordination function is stable but the extractive component is growing — Goodhart drift where priestly claims shift from 'rituals maintain cosmic order' toward 'temple complexes need more wealth to maintain order' — the actual function shifts from coordination to rent-seeking while the justifying narrative stays constant.
 *
 * DIRECTIONALITY LOGIC:
 *   Priestly class and temple economies are structural beneficiaries (d approaching 0.0 for both: they collect wealth and authority under the constraint and frame it as necessary). Pharaoh sits near symmetric (d ~0.5): gains legitimacy from priestly validation but loses autonomy; genuine coordination benefit paired with real constraint on pharaonic power. Regional cults and non-conforming believers are targets (d approaching 1.0: they lose religious autonomy and wealth flows away from their institutions). Common subjects are near-beneficiary (d ~0.25-0.35: genuine coordination benefit — explanatory framework, ritual structure — but diffuse costs through labor obligation and religious conformity pressure). Rival factions (Atenists, syncretists) are trapped targets (d ~0.9: the constraint directly suppresses their theology and they lack power to exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope to capture the genuine coordination function (pharaonic succession mechanism, cosmological coherence, ritual calendar) that is paired with substantial asymmetric extraction. If classified as rope alone, the analysis would miss that the extraction persists because temples actively suppress alternatives and because the pharaoh is trapped into funding temples under threat of withdrawn legitimacy. If classified as snare alone, the analysis would miss that the coordination genuinely solves the succession problem and that pharaonic rule would be administratively incoherent without this constraint. Tangled_rope captures the structure: coordination + enforcement + asymmetric benefit = a hybrid where both aspects are real and both aspects drive persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    priestly_interpretation_as_genuine_authority_vs_power_consolidation,
    'Is the priestly claim to interpret divine will a genuine epistemological/theological authority, or is it a power consolidation strategy using theology as cover?',
    'Textual analysis of priestly writings to determine whether interpretation shows genuine theological reasoning or purely self-serving rent claims; comparison of priestly theology across dynastic periods to assess continuity vs. strategic revision; analysis of non-priestly theological texts (syncretistic, folk) to determine whether they show comparable theological sophistication (suggesting the priestly claim to interpretive authority is exclusionary rather than epistemically warranted).',
    'If genuine authority: the extraction is the necessary cost of the coordination function, and the constraint shifts toward rope classification (coordination with incidental extraction). If power consolidation: the extraction is the primary function and theology is the justification, and the constraint remains squarely tangled_rope or shifts toward snare (extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_interpretation_as_genuine_authority_vs_power_consolidation, conceptual, 'Whether priestly interpretive claims rest on genuine theological reasoning or are primarily a strategy for consolidating institutional wealth and power.').

omega_variable(
    suppression_of_alternatives_as_cosmological_necessity_vs_institutional_defense,
    'Is the active suppression of Atenist and syncretistic alternatives justified by the claim that cosmological disorder (chaos/isfet) results if the Amun-Ra hierarchy is not maintained, or is suppression primarily institutional self-defense?',
    'Textual analysis of priestly cosmological claims to determine whether they genuinely entail that alternative theologies would cause cosmic disorder, or whether suppression is asserted as institutional necessity regardless of cosmological claims; observation of actual cosmological or administrative disruption during periods when alternative readings gain ground (Akhenaten''s reign, post-New Kingdom priestly fragmentation) to assess whether the claimed consequences occur.',
    'If cosmological necessity: suppression serves the genuine coordination function and extraction is an entailment of maintaining cosmic order — the constraint remains tangled_rope but the extraction component is justified as coordination cost. If institutional defense: suppression is a pure control mechanism to protect the priestly monopoly — the constraint becomes more snare-like, with extraction as the primary driver and cosmological claims as post-hoc justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives_as_cosmological_necessity_vs_institutional_defense, empirical, 'Whether the suppression of alternative theological readings follows from the cosmological doctrines or serves primarily to protect priestly institutional interests.').

omega_variable(
    pharaonic_autonomy_genuine_constraint_vs_rhetorical_deference,
    'Is the pharaoh''s dependence on priestly legitimation a genuine structural constraint on pharaonic power, or is it rhetorical deference by the pharaoh to maintain stability while retaining autonomous decision-making?',
    'Analysis of pharaonic decrees and administrative records to determine whether pharaohs actually defer to priestly opposition (instances of blocked pharaonic initiatives due to priestly resistance); comparison of pharaonic autonomy across periods with varying priestly wealth/organization (weak priestly class vs. consolidated temple economies) to assess whether autonomy scales with priestly power; analysis of succession disputes and civil conflicts to determine whether priestly support is actually decisive in outcome.',
    'If genuine constraint: the pharaoh is a true payer and the constraint is tangled_rope with asymmetric extraction targeting pharaonic autonomy. If rhetorical deference: the pharaoh is a sophisticated beneficiary using priestly claims to legitimize pharaonic rule while retaining actual autonomy — the constraint shifts toward rope classification and pharaonic role shifts from payer to beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pharaonic_autonomy_genuine_constraint_vs_rhetorical_deference, empirical, 'Whether priestly legitimacy is a genuine constraint on pharaonic decision-making or a useful rhetorical tool the pharaoh deploys while retaining autonomy.').

omega_variable(
    kernel_reading_specificity_polytheistic_vs_monotheistic_boundary,
    'Is the boundary between polytheistic (Amun-Ra centered) and monotheistic (Aten exclusive) readings a fundamental epistemological divide, or do they represent different institutional configurations of roughly equivalent theological content?',
    'Textual and theological analysis comparing the Amun-Ra cosmology''s underlying monotheistic elements (Amun as hidden, transcendent, creator-of-all) with Atenism to assess whether the difference is one of explicit vs. implicit monotheism or genuinely distinct theological structures; analysis of elite theological literacy to determine whether educated priests understood Amun-Ra theology as implicitly monotheistic and viewed Atenism as merely making explicit what was already entailed.',
    'If genuine divide: the readings are truly incompatible and the constraint''s reading-specific structure is stable. If equivalent institutional configurations: both readings could coexist without foreclosure, suggesting the coexists_with relation between them is overstated and potentially a forecloses relation depending on how the kernel is specified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_specificity_polytheistic_vs_monotheistic_boundary, conceptual, 'Whether polytheistic and monotheistic readings of Egyptian divine legitimacy are fundamentally incompatible or represent different institutional expressions of similar theological content.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(divi_tr_t0, observed).
narrative_ontology:measurement(divi_tr_t7, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 7, 0.32).
narrative_ontology:measurement_basis(divi_tr_t7, observed).
narrative_ontology:measurement(divi_tr_t14, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 14, 0.37).
narrative_ontology:measurement_basis(divi_tr_t14, observed).
narrative_ontology:measurement(divi_tr_t21, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 21, 0.41).
narrative_ontology:measurement_basis(divi_tr_t21, observed).
narrative_ontology:measurement(divi_tr_t35, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 35, 0.43).
narrative_ontology:measurement_basis(divi_tr_t35, observed).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement_basis(divi_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(divi_be_t0, observed).
narrative_ontology:measurement(divi_be_t7, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 7, 0.54).
narrative_ontology:measurement_basis(divi_be_t7, observed).
narrative_ontology:measurement(divi_be_t14, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 14, 0.59).
narrative_ontology:measurement_basis(divi_be_t14, observed).
narrative_ontology:measurement(divi_be_t21, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 21, 0.62).
narrative_ontology:measurement_basis(divi_be_t21, observed).
narrative_ontology:measurement(divi_be_t35, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(divi_be_t35, observed).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(divi_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(divi_su_t0, observed).
narrative_ontology:measurement(divi_su_t7, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 7, 0.61).
narrative_ontology:measurement_basis(divi_su_t7, observed).
narrative_ontology:measurement(divi_su_t14, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 14, 0.66).
narrative_ontology:measurement_basis(divi_su_t14, observed).
narrative_ontology:measurement(divi_su_t21, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 21, 0.69).
narrative_ontology:measurement_basis(divi_su_t21, observed).
narrative_ontology:measurement(divi_su_t35, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(divi_su_t35, observed).
narrative_ontology:measurement(divi_su_t50, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(divi_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__amun_polytheistic_reading, 0.12).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_succession_mechanism_egypt).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, temple_economy_accumulation_new_kingdom).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested divine legitimacy kernel instantiating multi-deity cosmology with priestly interpretation and Amun-Ra supremacy. Sibling readings (atenist_monotheistic and folk_syncretistic) provide alternative framings of the same underlying problem of grounding pharaonic legitimacy. The kernel is the standing commitment: how divine will validates pharaonic rule. The reading specifies which mechanism (polytheistic priesthood vs. pharaonic revelation vs. folk pragmatism) carries the authority. Each reading has distinct ε, distinct stakeholder roles, and distinct suppression targets. This reading's increasing theater ratio and rising suppression over the interval suggests the coordination function is being captured by institutional extraction — a key diagnostic signal the engine produces through temporal measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__amun_polytheistic_reading, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
