% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__commemorative_husk_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: aneyoshi_stone_directive__commemorative_husk_reading
 *   human_readable: Aneyoshi Tsunami Stone Directive — Commemorative Husk Reading
 *   domain: disaster anthropology / institutional memory / land-use governance
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone, inscribed after the 1933 Showa Sanriku
 *   tsunami with an explicit warning not to build homes below its marked
 *   elevation, stood largely unchallenged for 78 years until the 2011 Tohoku
 *   tsunami validated its line almost exactly. Between 1933 and 2011,
 *   however, no institutional mechanism reinforced the directive: no zoning
 *   ordinance, no periodic ceremony of re-commitment, no living survivor
 *   testimony past a certain point. This reading holds that during that
 *   inter-catastrophe period the stone's function shifted — without anyone
 *   deciding it should — from a binding land-use constraint into a memorial
 *   object that people photographed, respected in the abstract, and built
 *   around in practice. The claim (piton) and the metrics (rising
 *   extractiveness and rising theater ratio) point the same direction here,
 *   which is itself notable: this is a case where the divergence-detection
 *   apparatus should find claim and metric largely aligned, in contrast to a
 *   false-summit case.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, 0.71).
domain_priors:suppression_score(aneyoshi_stone_directive__commemorative_husk_reading, 0.28).
domain_priors:theater_ratio(aneyoshi_stone_directive__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__commemorative_husk_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__commemorative_husk_reading, piton).
narrative_ontology:human_readable(aneyoshi_stone_directive__commemorative_husk_reading, "Aneyoshi Tsunami Stone Directive — Commemorative Husk Reading").
narrative_ontology:topic_domain(aneyoshi_stone_directive__commemorative_husk_reading, "disaster anthropology / institutional memory / land-use governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__commemorative_husk_reading, '5f2bfec7-d71d-4eff-9c49-0184da83ce61').
narrative_ontology:cs_kernel_codification('5f2bfec7-d71d-4eff-9c49-0184da83ce61', fixed_text).
narrative_ontology:cs_authority_grounding('5f2bfec7-d71d-4eff-9c49-0184da83ce61', practice).
narrative_ontology:cs_reading_relation('5f2bfec7-d71d-4eff-9c49-0184da83ce61', aneyoshi_stone_directive__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('5f2bfec7-d71d-4eff-9c49-0184da83ce61', foundational, unreinforced_inscription_decays_without_living_authority).
narrative_ontology:cs_axiom_status(unreinforced_inscription_decays_without_living_authority, holdable).
narrative_ontology:cs_axiom_grounding('5f2bfec7-d71d-4eff-9c49-0184da83ce61', unreinforced_inscription_decays_without_living_authority, empirically_contingent).
narrative_ontology:cs_axiom('5f2bfec7-d71d-4eff-9c49-0184da83ce61', secondary, commemoration_and_behavioral_compliance_are_structurally_distinct_functions).
narrative_ontology:cs_axiom_status(commemoration_and_behavioral_compliance_are_structurally_distinct_functions, holdable).
narrative_ontology:cs_axiom_grounding('5f2bfec7-d71d-4eff-9c49-0184da83ce61', commemoration_and_behavioral_compliance_are_structurally_distinct_functions, conventional).
narrative_ontology:cs_reference_frame('5f2bfec7-d71d-4eff-9c49-0184da83ce61', survivor_inscribed_permanent_injunction).
narrative_ontology:cs_drift_state('5f2bfec7-d71d-4eff-9c49-0184da83ce61', pre_2011_tohoku_tsunami, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5f2bfec7-d71d-4eff-9c49-0184da83ce61', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__commemorative_husk_reading, tourism_and_heritage_administrators).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, future_coastal_residents).
narrative_ontology:constraint_victim(aneyoshi_stone_directive__commemorative_husk_reading, prefectural_disaster_planners).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive__commemorative_husk_reading, intergenerational_warning_transmission_is_fragile).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Landowners, construction firms, and municipal planners who want to build below the stone's marked line. Under this reading, the stone has become a monument rather than an enforceable line, so they can point to decades of quiet non-compliance as evidence the directive no longer binds anything. They gain approvals, tax base, and buildable land whenever the directive is treated as symbolic.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests, beneficiary,
    organized, biographical, mobile, local).

% Local heritage bodies and disaster-tourism operators who maintain the stone as a photographed, narrated site. They benefit from the stone's continued physical presence as an artifact of memory and resilience storytelling, which requires no behavioral compliance from anyone — the stone photographs the same whether or not houses stand below its line.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, tourism_and_heritage_administrators, beneficiary,
    moderate, biographical, mobile, regional).

% Households who will move into or inherit property below the marked line during the current inter-catastrophe lull, without living memory of the 1896 and 1933 tsunamis the stone commemorates. They bear the full cost of the directive's decayed behavioral force the next time water returns, having had no voice in the drift from binding rule to commemorative object.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, local).

% Regional government bodies responsible for tsunami risk zoning who must now justify hazard maps without the stone's line carrying independent normative force. They pay in credibility and planning capacity: any hazard restriction they impose is now argued on hydrological modeling alone, since the stone's authority as lived directive has eroded into a heritage plaque.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, prefectural_disaster_planners, payer,
    institutional, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__commemorative_husk_reading, prefectural_disaster_planners, observer).

% The generation that carved and sited the marker explicitly stating 'do not build homes below this point' are no longer present to renew, reinterpret, or re-assert the directive's binding intent against decades of quiet reoccupation of the marked zone.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, the_1933_survivors_who_placed_the_stone, excluded,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(aneyoshi_stone_directive__commemorative_husk_reading, the_1933_survivors_who_placed_the_stone).

% Researchers who document how the stone's function shifted from lived warning to commemorated object across a multi-generational quiet period, without living authority to arbitrate which reading — binding rule or memorial artifact — is operative at any given time.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__commemorative_husk_reading, disaster_historians_and_ethnographers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_directive__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(aneyoshi_stone_directive__commemorative_husk_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its origin, the stone coordinated settlement location decisions across generations by encoding a hard-won empirical boundary (tsunami inundation line) into a durable, unambiguous physical marker requiring no institutional maintenance to persist.
% TRANSFER_FUNCTION: Under this reading, the arrangement now transfers risk from present-day development interests (who capture buildable land and tax revenue) to future residents (who inherit exposure), while transferring reputational and narrative capital to heritage/tourism administrators who benefit from the stone's continued photogenic presence without needing anyone to obey it.
% ABSENT_VOICES: The 1933 survivor generation who placed the stone cannot testify to whether they intended a permanent behavioral injunction or a one-generation warning; their absence is structural, not incidental — the stone's text is unusually explicit, but the tradition of oral reinforcement that would have kept its force alive did not survive them.
% DISAPPEARANCE_RATIONALE: Development interests would argue removal of the stone changes nothing, since it already carries no enforceable force under this reading — houses already stand below the marked line in nearby communities. Disaster planners and future-residents advocates would argue its disappearance would remove the last physical anchor for any renewed hazard-zoning argument, accelerating buildout in the inundation zone. The verdict is genuinely contested between the parties, not settled by this reading alone.
% FOUNDING_PROBLEM: Repeated tsunami inundation of the same coastal terrace (1896, then again in 1933) prompted survivors to mark, in stone, the exact point above which rebuilding should occur — a low-cost, illiteracy-proof, institution-independent warning meant to outlast any single generation's memory.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians and ethnographers (outside the beneficiary set) attest that the stone's original text explicitly requested compliance beyond living memory and that inundation-zone construction has resumed nearby; prefectural disaster planners corroborate that the marked line no longer functions as an independent planning constraint. Development interests and heritage administrators, who benefit from the husk reading, are the only parties asserting the directive's behavioral force is legitimately obsolete — a claim this reading treats as descriptively accurate but structurally self-serving to note.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__commemorative_husk_reading, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__commemorative_husk_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__commemorative_husk_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_stone_directive__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(aneyoshi_stone_directive__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as rising from near-zero (0.08) at the stone's placement to substantial (0.71) by 2011, tracking the accumulation of construction and land-value capture in the marked hazard zone as the directive's living authority faded. Theater ratio rises even faster (0.05 to 0.82) because the stone's *social* function — commemoration, tourism, civic pride — was increasingly all that remained active, while its *behavioral* function (keeping the zone unbuilt) atrophied. Suppression is authored low (0.28) deliberately: under this reading there is no active coercive apparatus forcing compliance or non-compliance — the whole point of the husk reading is that the directive persists through inertia and reverence, not enforcement, which is precisely what distinguishes piton from tangled_rope or snare here.
 *
 * PERSPECTIVAL GAP:
 *   From development interests' seat, the arrangement looks like harmless heritage preservation with no ongoing behavioral demand — a rope, if anything, coordinating memory without imposing cost. From prefectural planners' and future residents' seats, the same structure looks like a slowly hollowed-out safety mechanism whose apparent stability (the stone still stands, still says the same thing) masks a total loss of the function that mattered. The engine's per-seat computation should register this asymmetry directly from the beneficiary/payer structural data, not from any adjudication this story makes about which seat is 'right.'
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal development interests and heritage/tourism administrators are authored as beneficiaries: the former capture buildable land and revenue precisely because the directive is read as non-binding; the latter capture ongoing cultural and touristic value from the stone's mere existence, which requires no one's compliance. Future coastal residents and prefectural disaster planners are authored as payers: the former inherit undisclosed physical risk, the latter inherit the credibility cost of having no independently authoritative marker to cite. The directionality here is stark because the same physical object produces gain for the historically distant, mobile, and organized actors, and cost for the temporally displaced (not-yet-arrived) and institutionally trapped actors.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a piton candidate rather than a mountain or a snare precisely because no party is positioned as an agenda_setter actively administering the directive — no one enforces it, and no one profits enough from actively maintaining its dormancy to count as a concentrated extractor (which would make it a snare instead). The beneficiaries gain from ambient decay, not from active administration; this is the diagnostic signature of piton over snare. Declaring mandatrophy here would assert the directive's original mandate (preventing rebuilding in the inundation zone) has outlived its institutional force while the artifact persists — which is exactly what the commemorative_husk reading claims, in contrast to the behavioral_competence reading's claim that the mandate silently held throughout.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unenforced_but_unviolated_ambiguity,
    'Does a directive that was never tested by construction pressure during a low-risk-perception period count as having ''retained behavioral force,'' or does absence of both compliance-testing and enforcement mean the force was already gone — the central disagreement between this reading and the behavioral_competence_reading of the same kernel?',
    'Comparative case analysis: were there any documented instances between 1933 and 2011 where the stone''s line was invoked to block or discourage a specific proposed construction? A record of even a few invocations would support the behavioral_competence reading; a total absence of documented invocations alongside gradual construction encroachment near (though not directly on) the marked zone would support the husk reading authored here.',
    'If invocation instances are found, this reading''s claim of a hollowed-out mandate would be substantially weakened and the constraint would need to be re-authored closer to a genuine rope or scaffold rather than a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenforced_but_unviolated_ambiguity, empirical, 'Whether behavioral force persisted untested or had already lapsed during the inter-catastrophe period — the kernel''s central contest.').

omega_variable(
    development_beneficiary_structure_natural_or_constructed,
    'Is the shift from binding directive to memorial artifact a natural consequence of generational memory decay (an inevitable feature of unreinforced oral/inscribed warnings), or was the decay actively assisted by development interests who benefited from treating the stone as merely symbolic?',
    'Archival review of municipal planning records and permit approvals in the marked zone between 1950-2011 to determine whether development interests actively lobbied against zoning codification of the stone''s line, versus the line simply never being proposed for codification by anyone.',
    'Active lobbying would sharpen this toward a tangled_rope or snare (identifiable extractive agency); pure passive drift without contestation supports the piton reading authored here, where no one profits enough from active maintenance to count as a concentrated extractor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(development_beneficiary_structure_natural_or_constructed, empirical, 'Whether the commemorative-husk drift was passively inherited or actively cultivated by development beneficiaries.').

omega_variable(
    kernel_framing_stone_vs_legitimacy_narrative,
    'Is the correct kernel object the stone-and-inscription itself, or the broader intergenerational-transmission legitimacy claim (''inscribed warnings can bind descendants'') that the stone is offered as evidence for? These two framings could produce different reading-relation structures.',
    'Compare classification outcomes treating the kernel narrowly (the physical marker''s specific instruction) versus broadly (the general doctrine that durable physical warnings substitute for institutional memory) — check whether the coexists_with/forecloses relations to the sibling reading change under the broader framing.',
    'Under the narrow framing (adopted here), the two readings coexist as competing empirical claims about one object''s history. Under the broad framing, the commemorative_husk reading could be read as actively undermining the general doctrine''s legitimacy, shifting the relation from coexists_with toward influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_stone_vs_legitimacy_narrative, conceptual, 'Alternative kernel-object framings (specific marker vs. general transmission doctrine) and their effect on reading-relation classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__commemorative_husk_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1933, 0.05).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 1990, 0.6).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2005, 0.75).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__commemorative_husk_reading, theater_ratio, 2011, 0.82).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1933, 0.08).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 1990, 0.51).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__commemorative_husk_reading, base_extractiveness, 2011, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__commemorative_husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(aneyoshi_stone_directive__commemorative_husk_reading, aneyoshi_stone_directive__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_directive__behavioral_competence_reading are the two declared readings of the aneyoshi_stone_directive kernel. Both share the same physical object, text, and 1933-2011 interval. They diverge entirely on whether the directive's behavioral force persisted through the inter-catastrophe lull (behavioral_competence, low epsilon, no beneficiary structure) or decayed into commemoration while development quietly resumed (commemorative_husk, high epsilon, development-interest beneficiary structure, authored here). Per the epsilon-invariance principle, these are authored as two separate constraints rather than one story with a contested epsilon value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
