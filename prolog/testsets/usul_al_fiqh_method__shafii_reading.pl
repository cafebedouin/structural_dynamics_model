% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Usul al-Fiqh Method: Hadith Authentication Prerequisite
 *   domain: legal/theological/epistemological
 *
 * SUMMARY:
 *   The Shafi'i reading of usul al-fiqh (principles of jurisprudence)
 *   systematizes Islamic legal sources into a hierarchical framework where
 *   hadith authentication becomes a prerequisite to legal derivation.
 *   Analogical reasoning (qiyas) is permitted only when authenticated hadith
 *   is absent, and consensus (ijma') is restricted to the Companions of the
 *   Prophet. This reading instantiates ONE interpretation of the contested
 *   kernel of how Islamic law derives from its sources. The Shafi'i reading
 *   transferred gatekeeping authority from rationalist jurists to hadith
 *   transmission specialists, positioning hadith scholars as the arbiters of
 *   what material counts as valid source evidence. This is ONE of four major
 *   competing readings—Hanafi, Maliki, and Hanbali readings subordinate or
 *   bypass hadith authentication in different ways. The Shafi'i reading is
 *   claimed as a coordination framework (unifying methodology) but operates
 *   with substantial extraction: it benefits hadith specialists and orthodox
 *   establishments while subordinating rationalist schools and independent
 *   jurists.
 *
 * KEY AGENTS:
 *   - hadith_transmission_specialists: Institutional gatekeepers; authenticate and classify hadith reports; their authority is prerequisite to legal derivation under this reading
 *   - rationalist_jurists: Powerful but constrained; their qiyas and ra'y methods are subordinated; they bear the cost of reduced independent authority
 *   - orthodox_juridical_establishment: Institutional beneficiary and co-agenda-setter; administers the hierarchy; has arbitrage to modify or negotiate within the framework
 *   - minority_methodological_schools: Moderate power, constrained exit; their methods are marginalized; they incur costs of subordinate institutional status
 *   - lay_believers: Organized beneficiaries; gain predictability and unified standard; mobile exit (can follow competing schools)
 *   - political_authorities: Observers with arbitrage; rely on the framework for legitimacy but can negotiate with rival schools or appeal to necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.71).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Usul al-Fiqh Method: Hadith Authentication Prerequisite").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "legal/theological/epistemological").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, 'e091610b-7c05-465c-bd21-0c178eef0d15').
narrative_ontology:cs_kernel_codification('e091610b-7c05-465c-bd21-0c178eef0d15', fixed_text).
narrative_ontology:cs_authority_grounding('e091610b-7c05-465c-bd21-0c178eef0d15', lineage).
narrative_ontology:cs_interpretation_layer_present('e091610b-7c05-465c-bd21-0c178eef0d15').
narrative_ontology:cs_reading_relation('e091610b-7c05-465c-bd21-0c178eef0d15', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('e091610b-7c05-465c-bd21-0c178eef0d15', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('e091610b-7c05-465c-bd21-0c178eef0d15', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('e091610b-7c05-465c-bd21-0c178eef0d15', foundational, hadith_authentication_prerequisite).
narrative_ontology:cs_axiom_status(hadith_authentication_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('e091610b-7c05-465c-bd21-0c178eef0d15', hadith_authentication_prerequisite, empirically_contingent).
narrative_ontology:cs_axiom('e091610b-7c05-465c-bd21-0c178eef0d15', foundational, qiyas_subordinate_to_hadith).
narrative_ontology:cs_axiom_status(qiyas_subordinate_to_hadith, holdable).
narrative_ontology:cs_axiom_grounding('e091610b-7c05-465c-bd21-0c178eef0d15', qiyas_subordinate_to_hadith, deontological).
narrative_ontology:cs_reference_frame('e091610b-7c05-465c-bd21-0c178eef0d15', hadith_prerequisite_source_hierarchy).
narrative_ontology:cs_drift_state('e091610b-7c05-465c-bd21-0c178eef0d15', contemporary_rationalist_jurisprudence_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e091610b-7c05-465c-bd21-0c178eef0d15', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, orthodox_juridical_establishment).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, minority_methodological_schools).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (early systematization) to 0.68 (institutional entrenchment) over the interval, plateauing as the framework becomes canonical. This trajectory reflects the constraint's lifecycle: initially presented as coordination (unified source methodology), it accumulates extractive features as hadith specialists consolidate gatekeeping and rationalist alternatives are progressively marginalized. Suppression rises in parallel (0.42 → 0.71), indicating increasing enforcement machinery (institutional pressure on schools, social costs for rationalist methods, exclusion mechanisms for weak transmitters). Theater ratio rises from 0.15 to 0.42 and then stabilizes, suggesting that as the framework matures, an increasing share of enforcement activity is performative (maintaining the hierarchy theatrically) rather than functional (solving the original coordination problem). The shared time grid ensures all metrics are authored at every time point (0, 20, 40, 60, 80, 120); no metric is omitted from any row, preventing OQ-105-style misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the hadith specialist seat, this is genuine coordination: a unified method that prevents chaos and enforces rigor. From the rationalist jurist seat, it is enforced subordination: their independent methods are arbitrarily demoted, and they must justify departures from hadith authentication despite prior authority to offer reasoned conclusions. The orthodox establishment sits between: it benefits institutionally but must invest in suppression to prevent rationalist and minority schools from reasserting independence. The engine will compute divergent seat-level types: beneficiary seats may see rope (coordination with side benefit), payer seats will see tangled_rope or snare (coordination framed as necessary, extraction actual), observer seats will detect the asymmetry. This divergence is precisely what the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (hadith specialists, orthodox establishment): receive gatekeeping authority, institutional primacy, and resource flows (students, patronage, official positions). Victims (rationalist jurists, minority schools): lose independent authority, must justify departures from hadith prerequisites, incur social costs and institutional marginalization. The transfer is epistemic, not monetary, but no less extractive—authority over source material is authority over the legal derivation process itself. Beneficiary directionality (d near 0.0) reflects that the framework subsidizes their expertise; victim directionality (d near 1.0) reflects that the framework extracts from their authority. The orthodox establishment's directionality is modulated by arbitrage: it can negotiate with competing schools, adopt hybrid methods, or appeal to necessity, pushing d toward the middle (0.4–0.6). The lay Islamic community's directionality is slightly beneficiary (d near 0.1–0.2): they gain predictability and unified standard, though the cost is reduced diversity of legal reasoning.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT mandatrophic at present: the founding problem (fabricated hadith, unsystematic rationalist derivation) remains contested, and the Shafi'i solution addresses a real coordination need even if it extracts substantially. However, the trajectory shows accumulating theater (rising from 0.15 to 0.42) and plateauing extractiveness (0.68, stable after time point 60). If theater continues rising above 0.50, the framework would approach piton status—functioning increasingly as institutional performance rather than as solution to the founding problem. The measurement series enables detection of this drift. If theater reaches 0.55+ and extractiveness remains high, the constraint would be reclassifiable as piton (atrophied coordination, pure inertia). Currently it is tangled_rope: genuine coordination function (unified source hierarchy solves real problem) PLUS asymmetric extraction (benefits hadith specialists, subordinates rationalists). The theater_ratio of 0.42 shows the coordination is still functional but increasingly theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentication_necessity_vs_sufficiency,
    'Is hadith authentication truly prerequisite to legal derivation in the Shafi''i framework, or is it a systematized SUFFICIENCY test that can coexist with other independent derivation methods?',
    'Comparative analysis of Shafi''i jurists who derived law when hadith sources were unavailable (pre-Shafi''i jurisprudential precedent, instances of necessity) versus Shafi''i jurists who explicitly rejected rationalist derivation as independent source. If Shafi''i jurisprudence contains functional qiyas and ra''y independent of hadith authentication, the framework is less prerequisite and more hierarchical preference.',
    'If authentication is truly prerequisite (not coexisting method), the constraint''s extraction is higher—it forecloses rationalist alternatives entirely. If it is sufficiency test (coexisting method), the constraint is less extractive—it subordinates but does not eliminate rationalist derivation. This affects the boundary between tangled_rope (coordination + extraction) and rope (coordination alone).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentication_necessity_vs_sufficiency, empirical, 'Whether hadith authentication is logically necessary or merely preferred in Shafi''i jurisprudence.').

omega_variable(
    internal_vs_external_suppression,
    'What fraction of the measured suppression is structural (institutional exclusion of weak transmitters, official gatekeeping by hadith scholars) versus internalized (jurists'' genuine belief in the framework''s epistemic superiority)?',
    'Historical analysis of jurist testimonies, philosophical defenses of usul al-fiqh, and counterfactual cases where jurists departed from the framework when external enforcement was absent (e.g., private correspondence, minority positions in diverse scholarly communities). Degree of principled adherence versus compliance under pressure indicates internalization ratio.',
    'High internalization (e.g., 70%+) suggests the framework''s legitimacy is well-grounded and suppression is self-reinforcing; low internalization (e.g., 30%) suggests institutional coercion is load-bearing and the framework would collapse if enforcement weakened. This affects the classification''s sustainability: high internalization makes the constraint more stable and more likely to appear natural; low internalization reveals the constraint as constructed and contingent on enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internal_vs_external_suppression, empirical, 'Ratio of internalized belief to structural coercion in suppression mechanism.').

omega_variable(
    competing_reading_foreclosure,
    'Does the Shafi''i reading logically foreclose (make impossible within any single framework) the Hanafi reading''s core premise that qiyas can be applied expansively, or do the readings coexist as different parties'' commitments without logical contradiction?',
    'Formal logical analysis: if a Shafi''i scholar granted qiyas''s applicability and validity (Hanafi premise), would they necessarily violate a Shafi''i committal axiom? Or can a single scholar hold both that hadith authentication is the preferred method AND that qiyas is valid when hadith is absent? If both are derivable within unified jurisprudential framework, readings coexist; if one premise directly contradicts the other, reading forecloses.',
    'Foreclosure relation would make the readings mutually exclusive in any framework; coexistence relation suggests institutional rivalry without logical necessity. Foreclosure supports a stricter boundary between Shafi''i and competing schools; coexistence suggests more fluid scholarly migration and hybrid positions. The relation determines how the engine models the constraint''s reach—does it exclude alternatives, or do alternatives persist alongside it?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether Shafi''i and Hanafi readings are logically incompatible or institutionally competing.').

omega_variable(
    beneficiary_solidarity_and_contestation,
    'Do hadith specialists and the orthodox juridical establishment benefit equally from the Shafi''i framework, or is there latent conflict between them (e.g., hadith scholars want to maximize gatekeeping authority, while the establishment wants controlled qiyas for flexibility)?',
    'Historical analysis of disputes within Shafi''i jurisprudence between hadith masters and jurist innovators, instances where the establishment endorsed qiyas over hadith specialists'' recommendations, and cases where hadith specialists'' exclusions were overridden for social or political reasons.',
    'Equal benefit: both are straightforward beneficiaries, and d values diverge from payers. Latent conflict: hadith specialists are beneficiaries (high authority), but the establishment is near-symmetric (benefits from hierarchy but needs flexibility to maintain institutional legitimacy). This affects the directionality override strategy—whether both beneficiaries can be treated identically or whether one requires override toward payer territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_solidarity_and_contestation, empirical, 'Degree of alignment between hadith specialists and orthodox establishment in benefiting from the framework.').

omega_variable(
    kernel_reading_contest_closure,
    'Is the contest between Shafi''i and competing readings genuinely open in contemporary Islamic jurisprudence, or has one reading achieved hegemonic closure such that alternatives are only formally recognized but not actually practiced?',
    'Institutional analysis of contemporary Islamic legal education, fatwa issuance, state-appointed qadi networks, and theological academy composition across Muslim-majority regions. If Shafi''i readings dominate institutional positions and resources while alternatives are geographically or socially marginalized, the reading contest is closed de facto even if formally open.',
    'Open contest: multiple readings remain live alternatives with institutional resources and scholarly authority. Closed contest: the Shafi''i reading has achieved hegemonic gatekeeping, and alternatives are historical artifacts or minority positions. This affects the constraint''s lifecycle classification—an open contest keeps alternatives visible and contestation active; closure moves the constraint toward inertial entrenchment (piton-adjacent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_closure, empirical, 'Whether the kernel reading contest remains institutionally open or has achieved de facto Shafi''i hegemony.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__shafii_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__shafii_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__shafii_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__shafii_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement(usul_tr_t120, usul_al_fiqh_method__shafii_reading, theater_ratio, 120, 0.42).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__shafii_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__shafii_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__shafii_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__shafii_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(usul_be_t120, usul_al_fiqh_method__shafii_reading, base_extractiveness, 120, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__shafii_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__shafii_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__shafii_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__shafii_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__shafii_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(usul_su_t120, usul_al_fiqh_method__shafii_reading, suppression_requirement, 120, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__shafii_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% The Shafi'i reading of usul al-fiqh is one of four coordinate instantiations of the contested kernel 'usul_al_fiqh_method'. Each sibling reading (Hanafi, Maliki, Hanbali) instantiates a different ε-invariant constraint with its own beneficiary/victim structure, suppression mechanism, and type classification. The readings share a common kernel (Islamic legal source hierarchy) but diverge in how they rank and constrain the sources. Network links connect all siblings; each story names the others in affects_constraints. The Shafi'i reading subordinates qiyas and elevates hadith authentication, creating institutional power for hadith specialists and constraints on rationalist jurists. The Hanafi reading expands qiyas and institutionalizes ra'y, creating power for rationalist jurists and constraints on strict textualists. These are structurally distinct constraint stories, not variations of one story. The readings influence each other (e.g., Hanafi dominance in Ottoman jurisprudence created institutional pressure on Shafi'i schools in those regions), but no single reading forecloses another within a unified Islamic jurisprudential framework—they coexist across different institutional, regional, and community contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
