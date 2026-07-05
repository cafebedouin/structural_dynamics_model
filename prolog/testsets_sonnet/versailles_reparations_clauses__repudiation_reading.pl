% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Regime — Repudiation Reading (Diktat Illegitimacy)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the repudiation reading of the Versailles
 *   reparations kernel: the position that the treaty was imposed under duress
 *   and is therefore void of binding force, leaving Germany with no
 *   obligation beyond token or voluntary gestures. This is a distinct
 *   constraint from the punitive_liability_reading (which holds Article 231
 *   grounds quasi-unlimited liability) and the limited_responsibility_reading
 *   (which holds obligations are real but bounded by capacity) — the three
 *   readings have different beneficiary/victim structures and different
 *   epsilon values and are authored as three separate stories linked through
 *   network.affects_constraints, not as one story with a measurement
 *   parameter. Historically the repudiation reading operated through
 *   escalating German non-cooperation (moratoria requests, the 1923 default
 *   on coal and timber deliveries, passive resistance in the occupied Ruhr)
 *   rather than through open denunciation of the treaty, which gives it a
 *   suppression profile that spikes during confrontation (1923) and partially
 *   subsides once renegotiated schedules (Dawes 1924, Young 1929) provided
 *   face-saving cover for continued reduced payment alongside the underlying
 *   illegitimacy claim.
 *
 * KEY AGENTS:
 *   - weimar_revisionist_bloc: agenda_setter (institutional/constrained) — administers the non-payment posture
 *   - german_heavy_industry: beneficiary (organized/mobile) — retains capital and capacity that would have transferred as reparations in kind
 *   - future_german_rearmament_planners: beneficiary (institutional/mobile) — downstream captor of freed fiscal and industrial space
 *   - french_and_belgian_war_claimants: primary target (powerful/trapped) — bears the nullified claim
 *   - allied_bondholders_and_taxpayers: secondary target (moderate/trapped) — absorbs the severed inter-Allied debt offset
 *   - german_domestic_wage_earners_under_hyperinflation: payer/excluded (powerless/trapped) — bears the acute cost of the confrontation strategy without a voice in choosing it
 *   - postwar_international_law_observers: analytical observer — assesses the duress claim against emerging treaty-voidability doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.87).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.71).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Regime — Repudiation Reading (Diktat Illegitimacy)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'ab19a039-71e0-4163-b0d1-528f8e21a2e9').
narrative_ontology:cs_kernel_codification('ab19a039-71e0-4163-b0d1-528f8e21a2e9', fixed_text).
narrative_ontology:cs_authority_grounding('ab19a039-71e0-4163-b0d1-528f8e21a2e9', distributed).
narrative_ontology:cs_reading_relation('ab19a039-71e0-4163-b0d1-528f8e21a2e9', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('ab19a039-71e0-4163-b0d1-528f8e21a2e9', versailles_reparations_clauses__limited_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('ab19a039-71e0-4163-b0d1-528f8e21a2e9', foundational, treaty_voidable_under_duress).
narrative_ontology:cs_axiom_status(treaty_voidable_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('ab19a039-71e0-4163-b0d1-528f8e21a2e9', treaty_voidable_under_duress, conventional).
narrative_ontology:cs_axiom('ab19a039-71e0-4163-b0d1-528f8e21a2e9', foundational, war_guilt_clause_lacks_moral_force).
narrative_ontology:cs_axiom_status(war_guilt_clause_lacks_moral_force, holdable).
narrative_ontology:cs_axiom_grounding('ab19a039-71e0-4163-b0d1-528f8e21a2e9', war_guilt_clause_lacks_moral_force, deontological).
narrative_ontology:cs_reference_frame('ab19a039-71e0-4163-b0d1-528f8e21a2e9', armistice_negotiated_settlement).
narrative_ontology:cs_drift_state('ab19a039-71e0-4163-b0d1-528f8e21a2e9', post_ruhr_occupation_hardening, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('ab19a039-71e0-4163-b0d1-528f8e21a2e9', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, weimar_revisionist_bloc).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_heavy_industry).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, future_german_rearmament_planners).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, french_and_belgian_war_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_bondholders_and_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_domestic_wage_earners_under_hyperinflation).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, coerced_treaties_lack_binding_force).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, war_guilt_clause_is_a_legal_fiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% German political and diplomatic elites (foreign ministry, nationalist parties, and later the Reich government) who characterize the treaty as a Diktat signed under threat of resumed invasion and blockade. They administer the actual reparations-refusal posture: defaulting on scheduled payments, provoking the Ruhr crisis, and negotiating each renegotiation (Dawes, Young) from a stance that the underlying obligation is void, not merely reduced.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, weimar_revisionist_bloc, agenda_setter,
    institutional, generational, constrained, national).

% Ruhr industrialists and steel cartels who benefit directly from non-payment: capital that would have been transferred abroad as reparations in kind (coal, machinery) instead stays inside German firms, and the repudiation framing supplies political cover for resisting Allied Control Commission inspections of production capacity.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_heavy_industry, beneficiary,
    organized, biographical, mobile, national).

% Military and industrial planners for whom every mark not transferred as reparations, and every treaty obligation successfully denied as illegitimate, expands the fiscal and material space eventually used to rebuild armed forces beyond treaty limits. They do not administer the repudiation argument but are the clearest downstream captors of the freed resources.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, future_german_rearmament_planners, beneficiary,
    institutional, generational, mobile, national).

% Governments and populations of the occupied and devastated northern French and Belgian territories who hold that the treaty obligation is the mechanism by which physical reconstruction and war debt were to be funded. Under the repudiation reading, their claims are nullified outright rather than merely reduced to capacity; they cannot compel payment once Germany's default is politically insulated by the illegitimacy argument, and occupying the Ruhr in 1923 is their only remaining coercive lever.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, french_and_belgian_war_claimants, payer,
    powerful, biographical, trapped, continental).

% British and French taxpayers and war-bond holders who financed the war on the expectation that German reparations would offset inter-Allied debt to the United States. The repudiation reading directly severs this expected transfer, leaving these obligations to be serviced from domestic taxation instead.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_bondholders_and_taxpayers, payer,
    moderate, biographical, trapped, continental).

% Ordinary German wage earners and savers whose currency was destroyed during the 1923 hyperinflation that accompanied the government's passive-resistance and default strategy in the Ruhr crisis. The repudiation reading is argued in their name (protecting the nation from ruinous transfer) but they bear the acute cost of the confrontation strategy through wiped-out savings and wages, while having no voice in whether repudiation or negotiated reduction was pursued.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_domestic_wage_earners_under_hyperinflation, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, german_domestic_wage_earners_under_hyperinflation, excluded).

% German and Allied economists (Keynes among external voices, plus German finance-ministry technocrats) who argued reparations should be scaled to actual capacity to pay rather than either the full claimed liability or wholesale repudiation. Their capacity-based framework is structurally excluded from the repudiation reading's binary of legitimate/illegitimate obligation — a middle position that treats Article 231 as legal formality gets no purchase once the treaty itself is declared void ab initio.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, rival_limited_responsibility_advocates, excluded,
    moderate, biographical, constrained, national).

% Later jurists and historians assessing whether duress at signing (the threatened resumption of blockade and invasion in 1919) meets the legal threshold for treaty voidability under emerging doctrines of coercion, and whether the repudiation reading is a legal argument or a political rationalization adopted after the fact to serve German interests.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, postwar_international_law_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine at the international level — the repudiation reading solves no multilateral coordination problem; if anything it defeats the coordination the treaty structure was meant to provide (an agreed, verifiable transfer schedule replacing unilateral seizure). The only coordination it performs is internal to Germany: consolidating domestic political consensus around non-payment.
% TRANSFER_FUNCTION: The reading functions to block a transfer that would otherwise run from Germany to France, Belgium, and (indirectly, via inter-Allied debt) Britain and the United States. Its structural effect is retention: capital, industrial capacity, and fiscal headroom that would have moved outward stay inside Germany, with a portion subsequently redirected toward industrial concentration and later rearmament rather than toward domestic reconstruction generally.
% ABSENT_VOICES: French and Belgian reconstruction claimants have no forum in which German domestic legitimacy arguments are adjudicated against their actual claims; the repudiation reading is argued entirely within German and later revisionist-international discourse, with claimant states represented only through the occupying-power leverage of 1923, which itself provoked the passive-resistance campaign that produced the hyperinflation borne by ordinary Germans.
% DISAPPEARANCE_RATIONALE: If the repudiation reading had not taken hold as the dominant German political posture, the treaty's fixed schedule (or a negotiated capacity-based reduction reached earlier and in good faith) would likely have produced a different reparations trajectory — plausibly avoiding the Ruhr occupation, the 1923 hyperinflation, and removing a major grievance narrative that revisionist and later National Socialist politics exploited. Its removal materially changes the interwar settlement's stability, not merely its interpretation.
% FOUNDING_PROBLEM: The felt problem the reading was built to solve, from the German side: an obligation set without German consent to a schedule exceeding plausible fiscal capacity, imposed under a treaty signed only under threat of renewed war — a claim of illegitimate coercion used to justify total non-payment rather than negotiated reduction.
% FOUNDING_PROBLEM_CORROBORATION: German nationalist politicians and later apologist historiography attest the duress claim as settled fact. Independent postwar international-law scholarship (including non-German legal historians examining the 1919 negotiating record) corroborates that genuine coercion was present at signing but disputes that duress voids the entire obligation rather than supporting renegotiation — meaning the corroboration outside the beneficiary set supports the narrower 'coercion occurred' claim but not the repudiation reading's leap to zero binding obligation.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.87 by 1933) because the repudiation reading's structural effect is complete suppression of the claimant states' compensation claims combined with capture of the freed resources by German industrial and rearmament interests — this is qualitatively different from a capacity-bounded reduction, which would show moderate rather than near-total extraction from the claimant side. Suppression is authored as elevated but volatile (spiking to 0.75 during the 1923 Ruhr crisis, partially subsiding under the Dawes and Young renegotiations, then rising again toward 1933 as the reading hardens into settled German political consensus) because the mechanism by which repudiation is enforced is confrontation and default rather than steady administrative extraction. Theater ratio rises across the interval (0.20 to 0.42) as the legal-illegitimacy argument increasingly does rhetorical work substituting for a genuine renegotiation the technocratic middle position (limited_responsibility_reading) would have offered. Accessibility collapse is authored moderate-low (0.35) because the alternative (capacity-bounded negotiated payment) remained visibly available and was actively argued throughout the period by excluded technocratic voices — the repudiation reading did not foreclose alternatives so much as politically outcompete them.
 *
 * DIRECTIONALITY LOGIC:
 *   The weimar_revisionist_bloc and german_heavy_industry sit near the beneficiary end: they administer or capture the retained resources. Rearmament planners are a downstream beneficiary despite not administering the argument directly. French and Belgian claimants and Allied bondholders sit near the full-target end: trapped exit, powerful but unable to compel payment once the illegitimacy framing insulates German default from straightforward enforcement. German wage earners under hyperinflation are a target embedded within the beneficiary's own polity — trapped, powerless, and bearing the acute cost of the confrontation strategy pursued nominally in their name, which is why they carry both payer and excluded roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coercive imposition without consent, at a schedule exceeding capacity) was genuinely live in 1919. By the mid-1920s, once the Dawes Plan restructured the schedule around demonstrated capacity, the narrower version of the German grievance was substantially addressed through negotiation rather than repudiation — yet the repudiation reading persisted and hardened rather than yielding to the capacity-based settlement, which is the founding_problem_status: contested signal. Treating this as a live, still-necessary illegitimacy claim after 1924 risks mislabeling continued strategic non-payment as principled resistance to coercion; treating the entire reparations claim as always illegitimate from 1919 forward would mislabel the genuine coordination and reconstruction-financing function the original treaty schedule (however harsh) was structurally trying to perform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_voidability_threshold,
    'Does the threatened resumption of blockade and invasion in 1919 meet the legal threshold under which coercion voids a treaty''s binding force, or does it constitute ordinary armistice-negotiation leverage that does not rise to treaty-voiding duress under the law of the time or under later-developed doctrine?',
    'Comparative analysis against later-codified treaty law (e.g., the Vienna Convention on the Law of Treaties Article 52 coercion standard, applied retrospectively as an interpretive lens) and against contemporaneous 1919 diplomatic practice for what counted as acceptable armistice pressure.',
    'If duress meets a genuine voidability threshold, the repudiation reading has real legal grounding independent of its self-interested German origin. If it does not, the reading is better characterized as a political rationalization for default dressed in legal language, which would push its structural classification further toward extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duress_voidability_threshold, conceptual, 'Whether 1919 armistice-threat coercion legally voids the treaty or is ordinary negotiating leverage.').

omega_variable(
    repudiation_vs_beneficiary_capture,
    'Is the repudiation reading a genuine principled legal position that happens to also benefit German industrial and rearmament interests, or is it primarily a beneficiary-interest position that adopted legal illegitimacy language as cover?',
    'Trace whether German advocacy for the repudiation reading tracked the legal duress argument''s strength over time, or tracked instead the material interests of industrial and military planners regardless of the legal argument''s merits (e.g., persistence of repudiation rhetoric even after Dawes-Plan renegotiation substantially addressed the capacity objection).',
    'If tracking shows interest-correlation rather than legal-argument-correlation, this strengthens the case that the reading functions as extraction cover rather than a genuine coordination-restoring legal correction, supporting the snare classification over a more sympathetic reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repudiation_vs_beneficiary_capture, empirical, 'Whether the reading''s persistence tracks legal argument strength or beneficiary interest.').

omega_variable(
    reading_framing_underdetermination,
    'Could the same historical record be honestly framed as this being fundamentally a claim about treaty VALIDITY (this story''s framing, kernel-level) versus fundamentally a claim about POST-HOC POLITICAL STRATEGY that used validity language instrumentally without any underlying legal commitment?',
    'Examine whether German negotiators and legal scholars in 1919-1921 (before the Ruhr crisis) argued duress-based invalidity consistently, or whether the invalidity framing emerged/hardened specifically as a strategic response to the 1923 occupation and hyperinflation crisis.',
    'If the invalidity framing hardened only after 1923, the cs_structure axioms below (treaty_voidable_under_duress) should be read as retroactively constructed rather than a stable foundational premise held from the treaty''s signing — this would not change the constraint''s classification but would affect confidence in the axiom''s grounding_type as empirically_contingent versus purely instrumental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the illegitimacy framing was a stable 1919 legal position or a retroactively hardened strategic narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1933).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.2).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1921, 0.28).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1923, 0.33).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1925, 0.36).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1929, 0.39).
narrative_ontology:measurement(vers_tr_t1933, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1933, 0.42).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.55).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1921, 0.62).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1923, 0.8).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1925, 0.83).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1929, 0.85).
narrative_ontology:measurement(vers_be_t1933, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1933, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.4).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1921, 0.5).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1923, 0.75).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1925, 0.6).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1929, 0.58).
narrative_ontology:measurement(vers_su_t1933, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1933, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the versailles_reparations_clauses kernel. punitive_liability_reading treats Article 231 as grounding quasi-unlimited German liability (highest claimant-favoring epsilon from the German-payer perspective). limited_responsibility_reading treats obligations as real but capacity-bounded (a moderate, negotiated-reduction epsilon). This repudiation_reading treats the entire obligation as void, producing the highest epsilon from the claimant-victim perspective (near-total suppression of French, Belgian, and Allied-bondholder claims) while simultaneously being the reading with maximal benefit concentration in German industrial and rearmament interests. The three are not the same constraint measured differently — they have different beneficiary/victim sets, different claimed types, and different persistence mechanisms, and are authored as three separate files per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
