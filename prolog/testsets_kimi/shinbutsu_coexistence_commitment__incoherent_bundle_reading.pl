% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle Maintained by Ambiguity and Power
 *   domain: religious studies/historical
 *
 * SUMMARY:
 *   This constraint story models the Edo-period institutional arrangement
 *   known as shinbutsu-shugo not as a coherent theological synthesis but as
 *   an incoherent bundle sustained by deliberate ambiguity and Tokugawa
 *   institutional power. The reading holds that the system avoided
 *   categorical questions (Are kami Buddhist manifestations? Are Buddhas
 *   foreign kami?) because answering them would have destabilized the
 *   political-religious equilibrium. The bundle collapsed under Meiji
 *   pressure not because Meiji invented categories, but because the state no
 *   longer needed the ambiguity and could profit from clarity. The constraint
 *   is authored as a tangled rope: it provided genuine coordination
 *   (preventing sectarian warfare, organizing funeral and festival life)
 *   while asymmetrically extracting doctrinal autonomy from Buddhist clergy
 *   and Shinto nativists in favor of the state and combined shrine-temple
 *   institutions.
 *
 * KEY AGENTS:
 *   - bakufu_state: Primary agenda-setter (institutional/constrained) â enforced ambiguity to prevent religious competition
 *   - shrine_temple_complexes: Primary beneficiary (institutional/constrained) â collected land, status, and ritual authority from the bundle
 *   - doctrinal_buddhist_clergy: Primary payer (moderate/constrained) â bore the cost of suppressed soteriological distinction
 *   - kokugaku_nativists: Secondary payer (moderate/constrained) â excluded from institutional power until the Meiji rupture
 *   - rural_worshipping_communities: Mixed payer/beneficiary (powerless/trapped) â received coordinated ritual but had no exit from ontological confusion
 *   - meiji_reformers: Analytical observer/terminal agent (institutional/analytical) â dismantled the constraint and exposed its incoherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.58).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle Maintained by Ambiguity and Power").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious studies/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e750801a-4788-4e7e-b995-cd3e545a1aeb').
narrative_ontology:cs_kernel_codification('e750801a-4788-4e7e-b995-cd3e545a1aeb', implicit).
narrative_ontology:cs_authority_grounding('e750801a-4788-4e7e-b995-cd3e545a1aeb', extraction).
narrative_ontology:cs_interpretation_layer_present('e750801a-4788-4e7e-b995-cd3e545a1aeb').
narrative_ontology:cs_reading_relation('e750801a-4788-4e7e-b995-cd3e545a1aeb', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('e750801a-4788-4e7e-b995-cd3e545a1aeb', shinbutsu_coexistence_commitment__domain_partition_reading, influences).
narrative_ontology:cs_axiom('e750801a-4788-4e7e-b995-cd3e545a1aeb', foundational, ambiguity_sustained_institutional_power).
narrative_ontology:cs_axiom_status(ambiguity_sustained_institutional_power, holdable).
narrative_ontology:cs_axiom_grounding('e750801a-4788-4e7e-b995-cd3e545a1aeb', ambiguity_sustained_institutional_power, empirically_contingent).
narrative_ontology:cs_axiom('e750801a-4788-4e7e-b995-cd3e545a1aeb', foundational, bunri_revealed_preexisting_incoherence).
narrative_ontology:cs_axiom_status(bunri_revealed_preexisting_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('e750801a-4788-4e7e-b995-cd3e545a1aeb', bunri_revealed_preexisting_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('e750801a-4788-4e7e-b995-cd3e545a1aeb', practiced_ambiguous_equilibrium).
narrative_ontology:cs_drift_state('e750801a-4788-4e7e-b995-cd3e545a1aeb', meiji_restoration, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('e750801a-4788-4e7e-b995-cd3e545a1aeb', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bakufu_state).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complexes).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, doctrinal_buddhist_clergy).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kokugaku_nativists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rural_worshipping_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rural_worshipping_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Tokugawa shogunate enforced temple registration and shrine oversight to prevent religious organizations from becoming competing military or political power centers. It sustained the ambiguity between kami and Buddhas to forestall sectarian conflict and maintain a unified ritual field under its regulatory control.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bakufu_state, agenda_setter,
    institutional, generational, constrained, national).

% Combined shrine-temple institutions administered overlapping rituals, shared sacred sites, and joint patronage networks. They received land tenure, tax exemptions, and social authority from the syncretic system, and they opportunistically deployed honji suijaku rhetoric without resolving its ontological tensions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_complexes, beneficiary,
    institutional, generational, constrained, national).

% Buddhist monks and scholars whose traditions required clear soteriological hierarchy found their doctrinal distinctions absorbed into local kami cults. They could not publicly assert Buddhist supremacy or pure-land exclusivism without risking state penalty and loss of institutional standing.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, doctrinal_buddhist_clergy, payer,
    moderate, biographical, constrained, national).

% Scholars of National Learning who asserted the divine descent of the imperial line and the autonomy of Shinto kami were marginalized during the Edo period. Their exclusion was structurally necessary to the Buddhist-dominant syncretism, and they gained institutional voice only after the Meiji rupture.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kokugaku_nativists, payer,
    moderate, generational, constrained, national).

% Peasant and commoner communities received coordinated funeral and festival services from the combined shrine-temple system, but their religious practice was conscripted into the institutional bundle. They had no access to doctrinal clarity and no legal or economic path to alternative religious frameworks.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rural_worshipping_communities, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rural_worshipping_communities, beneficiary).

% Meiji oligarchs and bureaucrats who dismantled the syncretic system via shinbutsu bunri decrees. They treated the ambiguity as a political liability, forcibly separated shrine and temple lands, and reorganized religious authority around imperial Shinto.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_reformers, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevented sectarian conflict and political competition between Buddhist institutions and Shinto shrines by submerging categorical theological differences under a shared, state-overseen ritual economy.
% TRANSFER_FUNCTION: Moved ontological clarity and categorical autonomy from doctrinal specialists and nativist scholars to the state and shrine-temple complexes, in exchange for ritual coordination and the suppression of open religious warfare.
% ABSENT_VOICES: Nativist Shinto scholars who advocated kami-only worship before Meiji; rigorous Buddhist sectarians who resisted the subordination of soteriology to local cults; commoners who might have sought clearer religious categories but were never consulted in the institutional design.
% DISAPPEARANCE_RATIONALE: When Meiji shinbutsu bunri forcibly resolved the ambiguity, shrine and temple lands were separated, priesthoods disentangled, funeral practice reorganized, and the entire religious-political economy restructured around categorical clarity. The prior equilibrium could not survive without the ambiguity.
% FOUNDING_PROBLEM: Post-Sengoku religious fragmentation and the threat of militant Buddhist sects to political stability; the need to integrate Shinto shrines into a Buddhist-dominated ritual order without provoking sectarian conflict.
% FOUNDING_PROBLEM_CORROBORATION: Meiji reformers and modern historians attest that the Tokugawa peace had eliminated militant sectarian violence long before the arrangement was dismantled. Tokugawa-era kokugaku critiques from outside the benefiting institutions argued the founding threat had been replaced by new problems (foreign pressure, imperial restoration), corroborating that the original problem no longer justified the constraint.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the arrangement systematically subordinated doctrinal clarity to institutional convenience; suppression (0.72) is higher because the bundle required active policing of categorical questions (temple registration, suppression of sectarian dispute). Theater ratio (0.48) reflects that syncretic practice was genuinely performed and socially embedded, but an increasing share of institutional activity went to maintaining the appearance of coherence as kokugaku pressure grew. Accessibility collapse (0.68) is high because once inside the system, pure Shinto or pure Buddhist alternatives were institutionally inaccessible. Resistance (0.55) is moderate: kokugaku scholarship and hidden Buddhist purism provided continuous friction, but open resistance was suppressed until the Tokugawa authority itself eroded. The measurement series share a single time grid (1600â1875) to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (bakufu) experienced the constraint as necessary governance preventing Sengoku-style religious warfare; the beneficiary seat (shrine-temple complexes) experienced it as institutional privilege and economic security. The payer seats (doctrinal clergy, nativists) experienced the same structure as systematic suppression of their categorical autonomy. The engine computes this divergence from the structural data: identical institutional scope but opposite directionality derived from beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The bakufu and shrine-temple complexes are declared beneficiaries, yielding low directionality (subsidy from the constraint). The doctrinal Buddhist clergy and kokugaku nativists are declared victims, yielding high directionality (target status). Rural worshipping communities sit near symmetric: they receive genuine coordination benefit (ritual services) while paying in ontological confusion and institutional subordination, captured by their dual payer/beneficiary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpost-Sengoku religious militarismâwas dead by the eighteenth century, yet the arrangement persisted because the state and shrine-temple complexes had become structurally dependent on it. This prevents mislabeling the constraint as a rope (it outlived its coordination necessity) or as a snare (it did supply genuine coordination while the founding problem was live). The Meiji collapse confirms mandatrophy: when an external authority stopped enforcing the ambiguity, the bundle immediately unraveled, revealing that its persistence had become inertial extraction rather than live problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_intentional_vs_emergent,
    'Was the religious incoherence deliberately maintained by Tokugawa institutional power, or did it emerge organically from grassroots practice independent of state design?',
    'Comparative archival analysis of bakufu edicts suppressing doctrinal dispute vs. demographic patterns of local worship showing syncretism predated or exceeded state design.',
    'If purely emergent, the extraction framing weakens and the constraint moves toward rope; if intentionally maintained by power, the tangled rope reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentional_vs_emergent, empirical, 'Whether ambiguity was state-engineered or culturally emergent').

omega_variable(
    meiji_bunri_creation_or_revelation,
    'Did Meiji shinbutsu bunri create the categorical separation it claimed to reveal, or did it expose categories that already existed in subordinated form?',
    'Pre-Meiji textual and ritual evidence of covert categorical distinction among certain clerics and shrines, weighed against evidence of seamless syncretism in regional practice.',
    'If pre-Meiji distinctions were widespread, the incoherent-bundle reading overstates the system''s uniformity; if they were absent, the reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_bunri_creation_or_revelation, empirical, 'Whether Meiji separation manufactured or revealed incoherence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 1600, 1875).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1600, 0.25).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1650, 0.32).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1700, 0.4).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1750, 0.48).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1800, 0.55).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1850, 0.65).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1875, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1875, 0.15).

% Extraction over time
narrative_ontology:measurement(shinbutsu_incoherent_be_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1600, 0.42).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1650, 0.5).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1700, 0.56).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1750, 0.59).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1800, 0.62).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1850, 0.58).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1875, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1875, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_incoherent_su_t1600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1650, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1650, 0.65).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1700, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1700, 0.72).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1750, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1750, 0.75).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1800, 0.7).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1850, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1850, 0.45).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1875, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 1875, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the shinbutsu_coexistence_commitment kernel, decomposed from the colloquial label shinbutsu-shugo per the epsilon-invariance principle. Sibling readings instantiate structurally distinct claims: syncretic_fusion_reading asserts ontological unification via honji suijaku, while domain_partition_reading asserts stable functional separation. This reading holds that neither coherence obtained historically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
