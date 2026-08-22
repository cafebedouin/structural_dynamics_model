% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method — 'Amal Ahl al-Madina as Living Source
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   The Maliki reading instantiates the jurisprudential_method_kernel by
 *   claiming that the living practice of the Medinan community ('amal ahl
 *   al-Madina) is a valid, independent source of law — not merely evidence
 *   for hadith but a parallel transmission of the Prophetic sunna. This claim
 *   structurally privileges the Medinan scholarly lineage and its
 *   institutional heirs across North and West Africa, while subordinating
 *   reasoning-based schools (especially Hanafi qiyas/istihsan) and
 *   text-critical schools (Shafi'i hadith hierarchy). The constraint operates
 *   as a tangled rope: it genuinely coordinates legal practice across vast
 *   regions and centuries (providing stability, reducing adjudicative
 *   variance) while simultaneously extracting epistemic authority from rival
 *   methodologies and excluding reformist ijtihad claims. The coordination
 *   function is real — Maliki courts from Timbuktu to Cairo operated with
 *   remarkable consistency for centuries — but the extraction is structural:
 *   the 'living tradition' claim cannot be falsified internally and functions
 *   as a closure mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.42).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.58).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method — 'Amal Ahl al-Madina as Living Source").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '687b06fe-9b2e-4426-a3ad-891bb52fb2eb').
narrative_ontology:cs_kernel_codification('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', distributed).
narrative_ontology:cs_authority_grounding('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', lineage).
narrative_ontology:cs_interpretation_layer_present('687b06fe-9b2e-4426-a3ad-891bb52fb2eb').
narrative_ontology:cs_reading_relation('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_reading_relation('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', foundational, medinan_amal_is_independent_hujja).
narrative_ontology:cs_axiom_status(medinan_amal_is_independent_hujja, holdable).
narrative_ontology:cs_axiom_grounding('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', medinan_amal_is_independent_hujja, conventional).
narrative_ontology:cs_axiom('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', foundational, living_transmission_authenticates_without_isnad).
narrative_ontology:cs_axiom_status(living_transmission_authenticates_without_isnad, holdable).
narrative_ontology:cs_axiom_grounding('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', living_transmission_authenticates_without_isnad, conventional).
narrative_ontology:cs_reference_frame('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', prophetic_community_continuity).
narrative_ontology:cs_drift_state('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', classical_usul_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('687b06fe-9b2e-4426-a3ad-891bb52fb2eb', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_ulema_institutions).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, west_african_maliki_establishments).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_traditions).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, qiyas_heavy_schools).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, reformed_ijtihad_proponents).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, medinan_practice_preserves_prophetic_sunna).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, living_community_transmission_authenticates_hadith).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__maliki_reading, istihsan_and_qiyas_are_secondary_to_amal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the chain of transmission ('amal) from the Medinan community through Malik ibn Anas and his students; authoritatively defines what counts as the living practice of Medina. Their interpretive authority is inseparable from their institutional identity — exit would mean abandoning the very framework that constitutes them as authorities.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).

% Madrasas, courts, and fatwa bodies across North and West Africa that operate within the Maliki framework. They benefit from the institutional stability and recognition that comes from a practice-anchored methodology. Exit to another school would require restructuring curricula, judicial appointments, and communal trust — possible but costly.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_ulema_institutions, beneficiary,
    organized, generational, constrained, continental).

% Centuries-old scholarly networks in Timbuktu, Kano, Fez, and Cairo that have made Maliki practice the infrastructure of social order. Their authority is fused with the school's claim to represent the Prophetic community's living continuity. The constraint is not external to them — it is the grammar of their institutional self-understanding.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, west_african_maliki_establishments, beneficiary,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, west_african_maliki_establishments, agenda_setter).

% Hanafi, Shafi'i, Hanbali, and other schools whose methodologies are treated as derivative or less authentic by the Maliki claim to unmediated Prophetic practice. They bear the cost of having their interpretive labor relegated to secondary status — their qiyas, istihsan, and hadith-criticism are framed as poor substitutes for Medina's living transmission. They cannot exit the comparative framework without abandoning the inter-school discourse entirely.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_traditions, payer,
    organized, biographical, constrained, global).

% Particularly the Hanafi school, whose extensive analogical reasoning (qiyas) and juristic preference (istihsan) are explicitly subordinated to Medinan 'amal in Maliki epistemology. They pay an epistemic tax: their reasoning is treated as conjecture where Medina's practice is treated as witness. They have more exit mobility than identity-locked actors — they can and do maintain their own internal coherence — but the comparative hierarchy is structurally imposed.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, qiyas_heavy_schools, payer,
    organized, biographical, mobile, global).

% Modern reformist voices arguing for renewed independent reasoning (ijtihad) unconstrained by school methodology. They are excluded from the traditional discourse by the very structure of taqlid (following a school) that the Maliki method helps sustain. Their exclusion is not accidental — the living-tradition claim functions as a closure mechanism against claims of direct access to sources.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, reformed_ijtihad_proponents, excluded,
    moderate, biographical, trapped, global).

% Academic and cross-school scholars who study the methodological differences without being bound by any school's internal authority. They see the full structural map: how each school's founding claim generates its own beneficiary/victim configuration. Their seat is analytical — they neither collect nor pay within the constraint.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, comparative_fiqh_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, community-validated interpretive anchor for Islamic law in regions where the Maliki school prevailed — avoiding the indeterminacy of open-ended analogical reasoning by tethering judgment to a concrete, transmitted practice.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional legitimacy from non-Medinan reasoning methods (qiyas, istihsan, formal hadith criticism) to the Medinan scholarly lineage and its institutional heirs, who claim unmediated access to the Prophetic sunna through living transmission.
% ABSENT_VOICES: Early Medinan dissenters (e.g., Ibn Shihab al-Zuhri's critics, the ahl al-ra'y in Medina itself) who contested whether 'amal represented consensus or a particular faction's practice. Also absent: the Prophet's companions who left Medina and established different practice communities (Kufa, Basra, Damascus) — their traditions are retrospectively subordinated. These voices are structurally excluded because the 'living tradition' claim requires a monolithic Medina.
% DISAPPEARANCE_RATIONALE: If the 'amal ahl al-Madina claim vanished, the Maliki school would lose its distinguishing epistemology — its courts, curricula, and fatwa bodies would need a new methodological anchor (likely shifting toward Shafi'i-style hadith hierarchy or Hanafi-style systematic reasoning). The West African scholarly establishment's self-understanding as heirs of the Prophetic community would fracture. The inter-school hierarchy of authenticity would collapse.
% FOUNDING_PROBLEM: After the Prophet's death, the Muslim community faced divergent practices across garrison cities (Kufa, Basra, Damascus, Fustat, Medina). The Medinan community claimed unique fidelity because the Prophet lived and died there, his companions remained there, and daily practice continued uninterrupted. The founding problem was: which community's practice reliably preserves the Prophetic sunna?
% FOUNDING_PROBLEM_CORROBORATION: Maliki sources (Ibn al-Qasim, al-Qarafi, al-Shatibi) attest the problem is live — Medina's practice remains the gold standard. Non-Medinian schools (Shafi'i's Risala, Hanafi usul works) and modern historians (Schacht, Hallaq, Lucas) corroborate that the 'Medinan monopoly' claim was contested from the 2nd/8th century onward — Kufan and Basran scholars explicitly rejected Medina's epistemological priority. The corroboration is split: the beneficiary lineage says live; the excluded traditions and external scholarship say the founding problem was a constructed claim, not a discovered fact.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).
:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the epistemic rent collected by the Medinan lineage: their practice is treated as witness while others' reasoning is treated as conjecture. Suppression (0.58) is moderate — the constraint doesn't physically prevent other schools from operating, but it structurally denies them equal authenticity in the inter-school hierarchy. Theater ratio (0.28) captures the growing gap between the claimed purity of 'living transmission' and the historical reality of doctrinal development within the school (e.g., al-Qarafi's systematization, colonial-era codification). Accessibility collapse (0.55) is partial — alternatives (Shafi'i, Hanafi methods) remain coherent and practiced, but they are framed as inferior within the Maliki frame. Resistance (0.45) reflects centuries of inter-school polemic and modern reformist challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the Medinian lineage's seat, the constraint is genuine coordination — it solves the problem of divergent practice by anchoring law in the community that knew the Prophet. From the Hanafi/Shafi'i seat, it is asymmetric extraction — their rigorous reasoning is demoted to conjecture. From the reformist seat, it is a snare — the living-tradition claim blocks direct access to sources. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Medinan scholarly lineage and its institutional heirs (West African establishments) are identity-locked beneficiaries: their authority is constituted by the constraint itself. Non-Medinan schools are payers with constrained to mobile exit — they maintain their own systems but pay an epistemic tax in the comparative hierarchy. Reformed ijtihad proponents are trapped: excluded by the taqlid structure the constraint helps sustain. The analytical observer sees the full map without paying or collecting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (which community preserves the sunna) was live in the 2nd/8th century. By the 6th/12th century, the inter-school discourse had stabilized — all schools accepted each other's validity. The Maliki claim to unique authenticity persisted as a mandate beyond its coordinating function. The constraint now extracts epistemic rent from a settled landscape. Mandatrophy is unresolved: the arrangement persists because the beneficiary institutions are identity-locked, not because the founding problem demands it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_monolith_assumption,
    'Was the early Medinan community''s practice actually monolithic, or does the ''amal construct a retrospective unity that obscures internal diversity?',
    'Comparative analysis of early Medinan legal traditions (Muwatta variants, Ibn Jurayj, early Malikis) vs. Kufan/Basran parallels to detect whether ''Medinan practice'' was a coherent single stream or a constructed canonical selection.',
    'If Medinan practice was diverse, the ''living tradition'' claim extracts authority by presenting a contested selection as consensus — raising extractiveness and supporting tangled_rope/snare classification. If genuinely monolithic, the coordination function is stronger and extraction lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(medinan_monolith_assumption, empirical, 'Whether the constraint''s foundational empirical claim (Medina''s unique fidelity) withstands historical scrutiny.').

omega_variable(
    coordination_extraction_boundary,
    'Is the epistemic closure against non-Medinan reasoning (qiyas, istihsan, ijtihad) structurally necessary for the coordination function, or is it extractive overhead?',
    'Counterfactual comparison: regions where Maliki practice operated without strong ''amal dogmatism (e.g., early Andalus, some Sufi-influenced courts) vs. regions where the ''amal claim was enforced as orthodoxy. Did coordination degrade without the closure?',
    'If coordination persists without the epistemic closure, the suppression of rival methodologies is pure extraction. If coordination collapses, the closure is the price of the coordination function — genuine tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s extraction component is separable from its coordination function.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds West African Maliki establishments to the ''amal claim — professional identity (career path), relational identity (lineage transmission), ideological identity (worldview), or institutional identity (organization-become-function)?',
    'Sociological study of contemporary Maliki ulema in Mauritania, Senegal, Mali: survey exit intentions, measure identity fusion scales, track institutional responses to reform pressures.',
    'If identity lock is primarily institutional (the organization has become its function), mandatrophy resolution is harder — the institution cannot reform without dissolving. If primarily ideological, generational shift may loosen it. This determines whether the constraint is a piton (inertial) or active snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'The mechanism of identity lock for the primary beneficiary institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 150, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maliki_reading_tr_t150, jurisprudential_method_kernel__maliki_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(maliki_reading_tr_t300, jurisprudential_method_kernel__maliki_reading, theater_ratio, 300, 0.18).
narrative_ontology:measurement(maliki_reading_tr_t500, jurisprudential_method_kernel__maliki_reading, theater_ratio, 500, 0.22).
narrative_ontology:measurement(maliki_reading_tr_t700, jurisprudential_method_kernel__maliki_reading, theater_ratio, 700, 0.25).
narrative_ontology:measurement(maliki_reading_tr_t900, jurisprudential_method_kernel__maliki_reading, theater_ratio, 900, 0.27).
narrative_ontology:measurement(maliki_reading_tr_t1200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1200, 0.28).

% Extraction over time
narrative_ontology:measurement(maliki_reading_be_t150, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 150, 0.25).
narrative_ontology:measurement(maliki_reading_be_t300, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 300, 0.32).
narrative_ontology:measurement(maliki_reading_be_t500, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 500, 0.38).
narrative_ontology:measurement(maliki_reading_be_t700, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 700, 0.4).
narrative_ontology:measurement(maliki_reading_be_t900, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 900, 0.41).
narrative_ontology:measurement(maliki_reading_be_t1200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1200, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(maliki_reading_su_t150, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement(maliki_reading_su_t300, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 300, 0.45).
narrative_ontology:measurement(maliki_reading_su_t500, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 500, 0.52).
narrative_ontology:measurement(maliki_reading_su_t700, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 700, 0.55).
narrative_ontology:measurement(maliki_reading_su_t900, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 900, 0.57).
narrative_ontology:measurement(maliki_reading_su_t1200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1200, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__maliki_reading, 0.1).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four readings with distinct ε values. Maliki reading: ε=0.42 (medium extraction on custom/practice). Hanafi reading: ε≈0.35 (lower extraction, reason as coordination). Shafi'i reading: ε≈0.3 (standardization as coordination). Hanbali reading: ε≈0.5 (high extraction via literalist closure). The Maliki claim to unique authenticity structurally subordinates the others — this is the network influence edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__maliki_reading, institutional, 0.1).
constraint_indexing:directionality_override(jurisprudential_method_kernel__maliki_reading, organized, 0.65).
constraint_indexing:directionality_override(jurisprudential_method_kernel__maliki_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
