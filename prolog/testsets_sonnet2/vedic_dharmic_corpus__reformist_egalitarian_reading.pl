% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist-Egalitarian Reading of the Vedic-Dharmic Corpus
 *   domain: religious_authority/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This story authors ONE reading within a contested kernel over the
 *   Vedic-Dharmic textual corpus: the reformist-egalitarian reading, which
 *   holds that constitutional equality principles govern legitimate textual
 *   interpretation, that caste hierarchy is a later historical accretion
 *   rather than an original scriptural essence, and that rational critique
 *   may supersede traditional interpretive authority. This reading has been
 *   substantially institutionalized through post-independence constitutional
 *   courts and reform-oriented temple administration, giving it real
 *   enforcement teeth (temple-entry rulings, anti-discrimination statutes,
 *   state-administered temple boards) rather than remaining a purely
 *   intellectual position. The extraction this reading itself generates runs
 *   toward orthodox hereditary institutions, whose traditional interpretive
 *   monopoly and ritual-gatekeeping income are curtailed by judicial
 *   reinterpretation — hence 'tangled_rope' rather than 'rope': there is a
 *   genuine coordination function (a shared, workable interpretive standard
 *   for a pluralistic constitutional state) bundled with real asymmetric cost
 *   imposed on orthodox lineages and ritual specialists, sustained by active
 *   state enforcement (court orders, statutory mandates, temple-board
 *   appointments).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.4).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist-Egalitarian Reading of the Vedic-Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious_authority/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '0e749245-e4c7-4cdf-971d-0eb7f7ef45e6').
narrative_ontology:cs_kernel_codification('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', fixed_text).
narrative_ontology:cs_authority_grounding('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', extraction).
narrative_ontology:cs_interpretation_layer_present('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6').
narrative_ontology:cs_reading_relation('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', foundational, textual_meaning_bound_by_constitutional_equality).
narrative_ontology:cs_axiom_status(textual_meaning_bound_by_constitutional_equality, holdable).
narrative_ontology:cs_axiom_grounding('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', textual_meaning_bound_by_constitutional_equality, conventional).
narrative_ontology:cs_axiom('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', foundational, caste_hierarchy_is_historical_accretion_not_essence).
narrative_ontology:cs_axiom_status(caste_hierarchy_is_historical_accretion_not_essence, holdable).
narrative_ontology:cs_axiom_grounding('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', caste_hierarchy_is_historical_accretion_not_essence, empirically_contingent).
narrative_ontology:cs_axiom('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', secondary, rational_critique_supersedes_traditional_authority).
narrative_ontology:cs_axiom_status(rational_critique_supersedes_traditional_authority, holdable).
narrative_ontology:cs_axiom_grounding('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', rational_critique_supersedes_traditional_authority, instrumental).
narrative_ontology:cs_reference_frame('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', textual_essentialist_hierarchy).
narrative_ontology:cs_drift_state('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', post_constitutional_independence_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0e749245-e4c7-4cdf-971d-0eb7f7ef45e6', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_legal_institutions).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_courts).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahmin_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditionalist_ritual_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, lay_practitioners_across_castes).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, lay_practitioners_across_castes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate whether temple entry restrictions, caste-based exclusion practices, and scriptural citations used to justify hierarchy survive constitutional equality review. They read textual meaning through the lens of fundamental rights and can strike down or reinterpret practices claimed as religiously mandated. Their rulings bind institutions regardless of theological objection.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Have historically borne the material and dignitary costs of caste hierarchy justified by appeals to scriptural authority. This reading gives them a textual and legal lever: hierarchy is reframed as historical accretion, not essence, which supports temple-entry litigation, reservation policy, and anti-discrimination enforcement. Their exit from caste stigma remains structurally limited outside this reframing succeeding in law and practice.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_rights_movements, beneficiary,
    organized, generational, constrained, national).

% Legislatures and reform-oriented religious bodies (e.g. temple boards under state administration) draft and enforce statutes requiring caste-neutral access and interpretation. They gain legitimacy and administrative reach by aligning textual interpretation with constitutional norms, and they enforce this alignment through licensing, temple-board appointments, and litigation support.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_legal_institutions, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_legal_institutions, agenda_setter).

% Hereditary priestly lineages and orthodox mathas whose interpretive authority and ritual monopoly rested on treating varna hierarchy as textually essential. Under this reading, their traditional claims to exclusive interpretive standing are treated as historically contingent and subject to judicial override. They lose enforcement power over doctrine and ritual access, and their exit is constrained by dependence on state-recognized temple administration and legal standing.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahmin_institutions, payer,
    organized, civilizational, constrained, national).

% Local priests and ritual functionaries whose livelihood and social standing depend on caste-linked ritual exclusivity. Legal reinterpretation erodes their claimed authority to gatekeep ritual participation, threatening income and status; they have limited alternative occupations tied to their specialized training.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditionalist_ritual_specialists, payer,
    moderate, biographical, constrained, regional).

% Ordinary worshippers gain expanded access to temples, texts, and ritual participation previously restricted by caste. Some upper-caste lay practitioners experience this as loss of exclusive access or status; most lower-caste practitioners experience it as removal of a barrier, though social enforcement of caste norms at the community level often persists despite legal change.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, lay_practitioners_across_castes, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, lay_practitioners_across_castes, payer).

% Scholars who hold that varna is textually essential and divinely ordained are largely excluded from shaping constitutional jurisprudence, which treats their reading as a historical claim subject to secular equality review rather than as an authoritative theological position with standing of its own.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditionalist_theologians, excluded,
    organized, civilizational, trapped, national).

% Historians and textual scholars assess whether caste hierarchy is demonstrably a later accretion onto the corpus or an early, integral feature — informing but not resolving the legal and theological contest.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive standard allowing courts, reform-oriented religious administrators, and a modern pluralistic state to adjudicate religious practice disputes using a single constitutional yardstick, rather than deferring endlessly to competing sectarian claims of textual authority.
% TRANSFER_FUNCTION: Moves interpretive and ritual authority away from hereditary priestly lineages and toward state-recognized courts and reform institutions; moves social standing and access away from those who benefited from caste exclusivity and toward previously excluded castes.
% ABSENT_VOICES: Orthodox theologians who hold caste hierarchy as scripturally essential are treated as making a historical rather than theological claim in court, and so are procedurally sidelined from the terms of the debate even when substantively present as litigants.
% DISAPPEARANCE_RATIONALE: If courts and reform institutions stopped requiring textual interpretation to conform to constitutional equality, caste-based temple exclusion, ritual gatekeeping, and hereditary interpretive monopolies would very likely re-consolidate in many jurisdictions absent independent enforcement; decades of anti-discrimination litigation and access rulings would lose their doctrinal foundation.
% FOUNDING_PROBLEM: Post-independence constitutional framers and reform movements confronted a religious corpus whose received readings were used to justify caste-based exclusion (temple entry bans, ritual disqualification, social ostracism) in a state constitutionally committed to equality before law.
% FOUNDING_PROBLEM_CORROBORATION: Dalit rights organizations and constitutional court rulings (e.g. temple-entry judgments) attest the founding problem remains live and substantially unresolved in social practice despite legal victories. Independent historians of caste corroborate that hierarchy hardened over centuries rather than being a fixed original feature, supporting the accretion claim from outside both the reformist and orthodox camps. Orthodox institutions dispute the characterization itself, which is the substance of the contest.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (~0.45) reflects genuine but moderate cost transfer: orthodox institutions lose interpretive monopoly and associated income/status, but this is a redistribution of previously exclusionary privilege rather than a novel extraction from a previously unburdened party — hence moderate rather than high. Suppression (~0.4) captures active enforcement (court orders, legal mandates) against continued caste-exclusionary practice, real but not maximal since orthodox institutions retain theological, associational, and some political voice. Theater ratio (~0.3) acknowledges that some enforcement is genuinely substantive (temple-entry access) while some is performative (formal non-discrimination policies without corresponding social change at the community level). Accessibility collapse is moderate (0.35): orthodox alternative readings remain available in private practice and theological discourse even where legally unenforceable in public institutions. Resistance is substantial (0.6): orthodox institutions litigate, organize, and theologically contest this reading continuously — this is a live, actively fought interpretive struggle, not a settled consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit rights movements and reform-legal institutions sit near the beneficiary end: they gain material access, dignity, and institutional standing from this reading's ascendance, though Dalit communities remain only partial beneficiaries given persistent informal caste enforcement outside formal legal reach. Orthodox Brahmin institutions and traditionalist ritual specialists sit near the target end: they bear concrete loss of interpretive monopoly, ritual income, and social status, with constrained exit since their specialized standing has no easy alternative institutional base. Constitutional courts and reformist legal institutions occupy an agenda-setting, non-extractive seat: they administer the reading rather than collecting from it directly, though their institutional legitimacy is bound up with its success.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a corpus historically read to justify caste exclusion within a state constitutionally committed to equality — remains genuinely contested rather than resolved: legal victories (temple entry, anti-discrimination statute) coexist with persistent informal caste enforcement at the community level, so the classification as tangled_rope (not scaffold) is deliberate — this is not a temporary transitional measure with a declared sunset, but an ongoing, actively enforced reinterpretation that both solves a real coordination problem (a workable common standard for a pluralist state) and imposes real, asymmetric ongoing costs on orthodox institutions. Treating it as a pure rope would erase the real losses borne by orthodox lineages and ritual specialists; treating it as a pure snare would erase the genuine coordination benefit and dignitary gain secured for previously excluded castes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accretion_vs_essence_historicity,
    'Is caste hierarchy demonstrably a later historical accretion onto the Vedic-Dharmic corpus, or is it textually original and integral, as the hereditary_monopoly_reading holds?',
    'Comparative philological and archaeological dating of caste-related textual strata against earlier corpus layers; convergence among independent historians outside both reformist and orthodox interest groups.',
    'If accretion is well-supported, the reformist reading''s coordination function (constitutional reinterpretation correcting a historical distortion) is strengthened; if hierarchy proves original, the reading is better understood as an external normative override rather than a restoration of authentic textual meaning, which would sharpen its extractive character toward orthodox institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accretion_vs_essence_historicity, empirical, 'Whether caste hierarchy is textually original or a later addition to the corpus.').

omega_variable(
    reformist_reading_own_extraction_ceiling,
    'Does state enforcement of this reading risk becoming its own extractive apparatus — e.g., state-administered temple boards accruing power or revenue beyond what correcting caste exclusion requires?',
    'Audit of temple-board administrative practices, revenue allocation, and appointment patterns for evidence of extraction beyond the stated equality-enforcement mandate.',
    'If state administration exceeds the corrective mandate, part of the measured extraction shifts from ''redistribution of orthodox privilege'' to a new extractive layer benefiting state-linked administrators, which would push the classification toward snare on the administrative dimension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformist_reading_own_extraction_ceiling, empirical, 'Whether state enforcement machinery for this reading generates its own independent extraction.').

omega_variable(
    kernel_framing_court_vs_theology,
    'Should this reading be assessed primarily as a legal-constitutional doctrine (courts as the authority) or as a theological claim (scriptural hermeneutics as the authority), given that its enforcement runs through courts but its legitimacy claim is about scriptural meaning?',
    'Track whether future contest over this reading is resolved primarily in constitutional courts (legal framing dominant) or in theological/scholarly forums (hermeneutic framing dominant) — the venue of resolution reveals which framing the relevant communities treat as authoritative.',
    'Under the legal framing, authority_grounding is closer to extraction/state-power (this story''s choice); under a theological framing, it would be closer to expertise (textual scholarship) with a different, likely lower, extraction profile since enforcement would be persuasive rather than coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_court_vs_theology, conceptual, 'Whether this reading''s authority is best modeled as juridical-state authority or as scholarly-theological authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(vedi_tr_t24, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(vedi_tr_t36, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 36, 0.27).
narrative_ontology:measurement(vedi_tr_t48, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 48, 0.29).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(vedi_tr_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 70, 0.3).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vedi_be_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(vedi_be_t24, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(vedi_be_t36, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 36, 0.43).
narrative_ontology:measurement(vedi_be_t48, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 48, 0.44).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(vedi_be_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 70, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(vedi_su_t12, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(vedi_su_t24, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(vedi_su_t36, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 36, 0.37).
narrative_ontology:measurement(vedi_su_t48, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 48, 0.39).
narrative_ontology:measurement(vedi_su_t60, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(vedi_su_t70, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 70, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.1).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the vedic_dharmic_corpus kernel. hereditary_monopoly_reading holds that varna hierarchy is divinely ordained and textually essential (this reading's direct doctrinal target — reformist_egalitarian_reading forecloses its core premise within any single legal framework, though both remain live in the broader social contest). bhakti_devotional_reading holds that sincere devotion bypasses caste independent of textual-legal reinterpretation (this reading coexists with and is influenced by the reformist reading's institutional success, since expanded legal access to worship spaces can create more room for devotional practice across castes, without either reading logically requiring the other).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
