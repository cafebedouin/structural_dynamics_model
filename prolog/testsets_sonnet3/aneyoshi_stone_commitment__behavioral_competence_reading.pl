% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone as Operative Land-Use Boundary (Behavioral-Competence Reading)
 *   domain: disaster anthropology / commitment systems / temporal institutional analysis
 *
 * SUMMARY:
 *   This story instantiates the behavioral-competence reading of the Aneyoshi
 *   tsunami-stone kernel: the claim that the stone's inscribed directive ('do
 *   not build below this point') retained genuine, causally operative force
 *   on land-use decisions across 78 years, and that the 2011 survival of
 *   every dwelling above the marker was a direct outcome of sustained
 *   compliance rather than coincidence or post-hoc storytelling. On this
 *   reading the stone functioned continuously as a low-overhead,
 *   self-enforcing land-use rule — a Rope, not a relic. The sibling story
 *   (commemorative_husk_reading) claims instead that the directive decayed
 *   into symbolic observance with no live behavioral constraint by the time
 *   of the 2011 event, making the survival outcome coincidental or
 *   attributable to other factors (terrain, road access, unrelated settlement
 *   drift). Both stories share the same referent — the standing Aneyoshi
 *   arrangement — and diverge only in what they claim about its operative
 *   status, per the ε-invariance and reading-indexed value principles.
 *
 * KEY AGENTS:
 *   - aneyoshi_residents: primary beneficiaries and practitioners of the norm (moderate/constrained) — bear no meaningful extraction, absorb the coordination benefit directly
 *   - descendant_households: dual beneficiary/agenda-setter role — they administer the norm through inherited practice with no formal office
 *   - prospective_lowland_developers: excluded/absent voice — the norm's social weight forecloses their position before it is articulated
 *   - municipal_and_national_disaster_planners: analytical observers — corroborate the founding problem's continued liveness from outside the benefiting household network
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.18).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Tsunami Stone as Operative Land-Use Boundary (Behavioral-Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster anthropology / commitment systems / temporal institutional analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '5981e4ce-ceb4-4d42-8fff-8b13996685fa').
narrative_ontology:cs_kernel_codification('5981e4ce-ceb4-4d42-8fff-8b13996685fa', implicit).
narrative_ontology:cs_authority_grounding('5981e4ce-ceb4-4d42-8fff-8b13996685fa', practice).
narrative_ontology:cs_interpretation_layer_present('5981e4ce-ceb4-4d42-8fff-8b13996685fa').
narrative_ontology:cs_reading_relation('5981e4ce-ceb4-4d42-8fff-8b13996685fa', aneyoshi_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('5981e4ce-ceb4-4d42-8fff-8b13996685fa', foundational, informal_norms_can_sustain_active_regulatory_force_without_formal_enforcement).
narrative_ontology:cs_axiom_status(informal_norms_can_sustain_active_regulatory_force_without_formal_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('5981e4ce-ceb4-4d42-8fff-8b13996685fa', informal_norms_can_sustain_active_regulatory_force_without_formal_enforcement, empirically_contingent).
narrative_ontology:cs_axiom('5981e4ce-ceb4-4d42-8fff-8b13996685fa', secondary, compliance_continuity_across_generations_evidences_causal_efficacy).
narrative_ontology:cs_axiom_status(compliance_continuity_across_generations_evidences_causal_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('5981e4ce-ceb4-4d42-8fff-8b13996685fa', compliance_continuity_across_generations_evidences_causal_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('5981e4ce-ceb4-4d42-8fff-8b13996685fa', post_1933_survivor_directive).
narrative_ontology:cs_drift_state('5981e4ce-ceb4-4d42-8fff-8b13996685fa', pre_2011_tsunami, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5981e4ce-ceb4-4d42-8fff-8b13996685fa', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, descendant_households).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_hazard_memory_efficacy).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, informal_norm_durability_across_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in a small Tohoku hamlet where a stone marker, erected after the 1933 Showa Sanriku tsunami, reads roughly 'do not build homes below this point.' Successive generations located dwellings uphill of the marker as a matter of ordinary practice, not active debate. In 2011 the tsunami stopped a short distance below the settlement; every house above the stone survived.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents, beneficiary,
    moderate, generational, constrained, local).

% Families who inherited both land and the informal expectation of building above the marker. They administer the norm by continuing to observe it in their own construction and land-transfer decisions, and by recounting the marker's history to children and newcomers, effectively renewing the directive's authority each generation without any formal office holding that role.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, descendant_households, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, descendant_households, agenda_setter).

% Outside parties or younger residents who might otherwise find land below the marker economically attractive (better road access, flatter terrain, lower cost) never seriously enter local discourse as a competing position; the norm's social weight forecloses the conversation before it starts. Their absence is not suppression by force but by an inherited near-consensus that leaves no live counter-argument articulated.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, prospective_lowland_developers, excluded,
    powerless, biographical, mobile, local).

% Japanese disaster-risk researchers and local government planners studied Aneyoshi after 2011 as a rare case of a folk hazard marker with a directly attributable survival outcome. They document, compare, and sometimes cite it in arguing for renewed tsunami-stone placement elsewhere, but they neither enforce nor administer the Aneyoshi marker itself.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, municipal_and_national_disaster_planners, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes a hazard boundary discovered through catastrophic experience (the 1933 tsunami's inundation line) into a durable, low-cost, non-bureaucratic land-use rule that successive generations can follow without needing to re-derive or re-verify the hazard data themselves.
% TRANSFER_FUNCTION: Transfers hazard knowledge across generations at near-zero cost: no monetary rent moves through the arrangement; what moves is inherited spatial discipline — the constraint on where a household may reasonably build passes from grandparent to grandchild as inherited practice rather than as legal instrument.
% ABSENT_VOICES: Anyone who might prefer to build on the cheaper, flatter land below the marker (a returning migrant unfamiliar with the 1933 event, a developer seeking coastal-adjacent lots) is structurally absent from the local conversation; the norm's near-total social acceptance means dissent is not suppressed so much as never voiced.
% DISAPPEARANCE_RATIONALE: If the stone and its associated practice vanished — physically removed and its social memory erased — nothing in current formal zoning law would replace it (Aneyoshi's building pattern was never captured in binding municipal ordinance); land below the marker would become buildable again in the eyes of residents and outside developers alike, and the settlement's spatial pattern would plausibly drift downhill within a generation or two, reintroducing the exposure the marker was built to prevent.
% FOUNDING_PROBLEM: After the 1933 Showa Sanriku tsunami killed the great majority of the hamlet's population, survivors needed a way to prevent their descendants from re-settling the flood-prone lowland once memory of the disaster faded, without relying on any government agency that might not persist or attend to so small a settlement.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 field surveys by Japanese seismological and disaster-risk institutions (outside the Aneyoshi household network itself) documented that the 2011 tsunami's run-up stopped below the marker and below every dwelling built in compliance with it, and cited the case in national policy discussions on hazard-marker preservation — corroboration from researchers with no stake in the local land-use pattern's continuation.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(aneyoshi_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.06 at 2011) because on this reading no party pays a rent through the arrangement — households constrained by the directive are the same households who benefit from the hazard protection it encodes; there is no asymmetric transfer to identify a victim class, which is why this story authors no victims array. Suppression is modest (0.18): the constraint is not maintained by coercion but by inherited social consensus, and the small rise across the measurement grid reflects increasing social solidification of the norm as it survived intact across generations without ever being tested until 2011. Theater ratio stays low throughout (0.02 to 0.10) — the modest late rise reflects the norm's gradual settling into ritualized retelling (the marker's story recounted at community gatherings) even while its behavioral force, on this reading, remained intact. Accessibility collapse is high (0.72): once a household internalizes the directive, building below the marker essentially stops being considered as a live option, which is precisely the coordination mechanism's chief evidence of operative force. Resistance is low (0.15): there was no active push against the norm to overcome — the near-total absence of a below-marker development lobby is itself part of what this reading claims to explain.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (aneyoshi_residents, descendant_households) sit near the full-beneficiary end of directionality: the arrangement subsidizes their safety at near-zero cost and asks only that they build where hazard memory says is safe, which coincides with what they would likely choose anyway once informed. No victim group is declared because, on this reading, the constraint imposes no meaningfully asymmetric cost on any identifiable party — the closest candidate, prospective_lowland_developers, is better modeled as excluded (a voice never entering the conversation) than as a payer bearing an extracted cost, since no transfer moves from them to anyone else.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing re-settlement of a lethal flood zone before disaster memory fades) is authored as still live in 2011 by the corroboration of independent post-tsunami surveys, which is what licenses classifying this arrangement as an active Rope rather than a mandatrophied Piton. The mismatch check that would flag capture or zombie status — founding_problem_status=dead paired with disappearance_verdict=world_rearranges — does not fire here: status is authored live, and disappearance_verdict is world_rearranges, which is the coherent combination for a constraint whose coordination function is still doing real work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_attribution_of_2011_survival,
    'Did compliance with the stone''s directive causally produce the 2011 survival pattern, or would the same dwellings have survived anyway due to terrain, road-access-driven settlement patterns, or other confounds unrelated to the marker''s behavioral force?',
    'Comparative analysis against nearby Sanriku settlements without an equivalent hazard marker but with similar terrain and inundation exposure; GIS reconstruction of the 1933 and 2011 inundation lines against actual building footprints and their construction dates to test whether siting decisions tracked the marker specifically versus tracking terrain incidentally correlated with the marker''s position.',
    'If terrain/access confounds fully explain the settlement pattern independent of the marker''s social authority, this reading''s core causal claim collapses and the commemorative_husk_reading becomes the better-supported account for the same period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_of_2011_survival, empirical, 'Whether the 2011 outcome is genuinely attributable to the stone''s operative force or to confounded terrain/access factors.').

omega_variable(
    reading_divergence_locus,
    'Where exactly does the behavioral-competence and commemorative-husk reading diverge — is it a factual dispute about whether households actually consulted or were guided by the marker in siting decisions, or an interpretive dispute about how much social force ''informal norm compliance without any enforcement mechanism'' can be said to carry?',
    'Oral history interviews with pre-2011 residents and descendants asking directly whether the marker was cited or considered during their own household''s building-location decisions, distinguished from post-2011 retrospective narrative construction (which both readings agree happened).',
    'If interviews show the marker was rarely or never consciously invoked in siting decisions before 2011, the behavioral-competence reading is substantially weakened even if the correlation between marker position and survival holds, because the causal chain (directive -> decision -> outcome) would be broken at the decision stage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_divergence_locus, conceptual, 'Whether the two kernel readings diverge on a resolvable empirical fact or on an irreducibly interpretive question about what counts as ''operative force.''').

omega_variable(
    generational_transmission_fidelity,
    'Across 78 years and roughly three to four generations, did the directive''s behavioral force remain constant, or did it fluctuate — strengthening after near-miss events, weakening during long quiet periods — such that ''retained operational force across 78 years'' understates real variance?',
    'Historical record search for any building permits, land disputes, or documented departures from the norm in the intervening decades that would reveal periods of weaker adherence.',
    'Evidence of fluctuating adherence would suggest the constraint''s type is better modeled as time-varying rather than as a single stable Rope classification across the full interval, and would qualify how strongly the 2011 outcome can be attributed to sustained rather than merely coincidentally-current compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_transmission_fidelity, empirical, 'Whether the norm''s behavioral force was constant or fluctuated meaningfully across the 78-year interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aney_tr_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1950, 0.03).
narrative_ontology:measurement(aney_tr_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(aney_tr_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1990, 0.06).
narrative_ontology:measurement(aney_tr_t2005, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2005, 0.08).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.1).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.03).
narrative_ontology:measurement(aney_be_t1950, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1950, 0.04).
narrative_ontology:measurement(aney_be_t1970, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1970, 0.04).
narrative_ontology:measurement(aney_be_t1990, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(aney_be_t2005, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2005, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_commitment__commemorative_husk_reading are the two declared readings of the aneyoshi_stone_commitment kernel. They share the same referent (the standing Aneyoshi marker-and-settlement arrangement) but author opposite claims about whether the directive retained causal behavioral force through 2011. This story authors extractiveness ~0.06 (Rope); the sibling authors a substantially different profile consistent with decayed, non-operative symbolic status. Both are reading-indexed per OQ-26; neither averages over the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
