% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading: State Militia Authority Protection
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The Second Amendment states: 'A well regulated Militia, being necessary
 *   to the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed.' The collective-right reading interprets
 *   this clause as protecting state governments' authority to maintain
 *   militia, not as securing an individual right to arms independent of
 *   militia service. Under this reading, states retain plenary power to
 *   regulate civilian gun ownership; individual claims to constitutional
 *   protection for arms possession outside the militia context have no
 *   ground. This constraint describes the standing arrangement when courts
 *   adopt the collective-right interpretation — the arrangement under
 *   contest, assessed by the reading's own lights. The sibling readings
 *   (individual-right, civic-republican) are separate constraints
 *   instantiating different interpretations of the same kernel text.
 *
 * KEY AGENTS:
 *   - State governments: institutional agenda-setters and beneficiaries; retain regulatory discretion over civilian arms
 *   - Individual gun owners outside militia: powerless payers; subject to state regulation with no constitutional protection
 *   - Federal courts adopting this reading: institutional agenda-setters; sustain state regulations by rejecting individual constitutional claims
 *   - Gun-rights advocates and organizations: excluded; their constitutional claim is foreclosed by the reading itself
 *   - Public safety and gun-control constituencies: beneficiaries; the reading legitimates broad regulatory regimes
 *   - Militia members in official state service: beneficiaries; protected arms access within their authorized role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.15).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.22).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment Collective Right Reading: State Militia Authority Protection").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '99a0c433-25d3-4532-887c-b809e525706c').
narrative_ontology:cs_kernel_codification('99a0c433-25d3-4532-887c-b809e525706c', fixed_text).
narrative_ontology:cs_authority_grounding('99a0c433-25d3-4532-887c-b809e525706c', lineage).
narrative_ontology:cs_interpretation_layer_present('99a0c433-25d3-4532-887c-b809e525706c').
narrative_ontology:cs_reading_relation('99a0c433-25d3-4532-887c-b809e525706c', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('99a0c433-25d3-4532-887c-b809e525706c', second_amendment_arms_right__civic_republican_reading, influences).
narrative_ontology:cs_axiom('99a0c433-25d3-4532-887c-b809e525706c', foundational, militia_right_supersedes_individual_claim).
narrative_ontology:cs_axiom_status(militia_right_supersedes_individual_claim, holdable).
narrative_ontology:cs_axiom_grounding('99a0c433-25d3-4532-887c-b809e525706c', militia_right_supersedes_individual_claim, deontological).
narrative_ontology:cs_axiom('99a0c433-25d3-4532-887c-b809e525706c', foundational, state_regulatory_authority_plenary_outside_militia_service).
narrative_ontology:cs_axiom_status(state_regulatory_authority_plenary_outside_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('99a0c433-25d3-4532-887c-b809e525706c', state_regulatory_authority_plenary_outside_militia_service, conventional).
narrative_ontology:cs_reference_frame('99a0c433-25d3-4532-887c-b809e525706c', state_militia_reserved_power).
narrative_ontology:cs_drift_state('99a0c433-25d3-4532-887c-b809e525706c', contemporary_federal_military_dominance, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('99a0c433-25d3-4532-887c-b809e525706c', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, militia_regulatory_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_national_guard_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, public_safety_authorities).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, constitutional_scholars_defending_collective_right).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, militia_as_constitutive_check_on_federal_power).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_sovereignty_in_defense_matters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State governments, through their legislatures and courts, operationalize the right to organize militia and regulate civilian arms. Under this reading, they hold the primary right and retain broad authority to design gun-control policies. They benefit from the reading's protection of state sovereignty in defense matters and from the legitimacy it confers on firearms regulation. They have options to adopt more or less restrictive policies; they can navigate between constitutional constraints (minimal under this reading) and political pressure.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__collective_right_reading, state_governments, agenda_setter).

% Individuals who wish to keep or bear arms outside state militia service have no constitutional protection under this reading. They are subject to whatever regulations state governments impose: licensing, registration, category bans, background checks. Their exit from this constraint is blocked by the constitutional interpretation itself — they cannot invoke the Second Amendment to challenge state regulation. They cannot migrate to a jurisdiction without guns because all states adopt similar frameworks under federal constitutional law.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia, payer,
    powerless, biographical, constrained, national).

% Federal judges and appellate courts that adopt the collective-right interpretation sustain state gun regulations and reject individual Second Amendment claims. They set the authoritative meaning of the constitutional text. They have the option to adopt the individual-right reading (as the Supreme Court did in DC v. Heller, 2008) and thereby overturn state regulations, or to sustain the collective reading and allow state regulatory latitude. Their analytical position permits them to change course, but precedent creates path dependence.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_courts_adopting_collective_reading, agenda_setter,
    institutional, generational, analytical, national).

% Members of state militia units have protected access to arms within their official service. They benefit from the reading's protection of militia armament as a state right. Their exit is constrained by their duty; they remain beneficiaries while serving. Their armed capacity is protected within the militia structure.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_national_guard_militia_members, beneficiary,
    moderate, biographical, constrained, national).

% Law enforcement, public health agencies, and gun-violence-prevention advocates benefit from the reading's permission for comprehensive gun regulation. Under this interpretation, they can design background-check systems, restrict high-capacity weapons, and implement permit regimes without constitutional interference. They face organized opposition from gun-rights groups, but the reading legitimates their regulatory approach as constitutionally sound.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, public_safety_authorities, beneficiary,
    institutional, generational, analytical, national).

% Organizations like the National Rifle Association and Second Amendment advocates argue for an individual-right reading. Under the collective-right reading, their constitutional claim is structurally excluded — the amendment does not protect what they claim it protects. Their exit from this constraint would require either constitutional amendment or successful judicial overturning of the collective-right reading. They have constrained exit by the power asymmetry: they cannot unilaterally change the constitutional interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_rights_advocates_and_organizations, excluded,
    organized, biographical, constrained, national).

% Historians and constitutional scholars (Saul Cornell, Carl Bogus, Cress) who defend the collective-right reading contribute interpretive authority and defend it against counter-arguments. They produce scholarship that sustains the reading's credibility. They have exit options through their disciplinary mobility; they can shift their interpretive position if evidence or arguments change their views. Their benefit is partly reputational (professional authority from correct historical interpretation) and partly normative (alignment with public-safety goals).
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, constitutional_scholars_defending_collective_right, beneficiary,
    analytical, biographical, arbitrage, national).

% The federal government does not directly hold rights under this reading but is implicitly constrained by the amendment's militia-check function — though that function is largely theoretical given federal military dominance. The federal government can propose amendments to alter the constraint, but doing so requires super-majority support. It holds an analytical position: it is neither a beneficiary nor a payer, but its military authority is the implicit referent of the militia-check rationale.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves state militia as a constitutionally recognized institution and protects state authority to arm and organize that militia independent of federal control. Coordinates defense capacity at the state level as a bulwark against federal military monopoly (though this check is now largely theoretical given federal dominance). Legitimates state firearms regulation as a proper exercise of state authority.
% TRANSFER_FUNCTION: Transfers constitutional protection for bearing arms from individuals (in non-militia contexts) to states and their militia institutions. The right shifts from the individual level to the institutional level. This reallocation does not move money or goods, but reallocates the burden of justification: individuals must justify why they should have arms access to state governments; states do not need federal justification to arm their militia. Under other readings (individual-right), the burden is reversed.
% ABSENT_VOICES: Individual gun owners who claim a pre-governmental right to arms are absent from the rights-holding set under this reading — their claim is foreclosed, not debated. Gun-rights advocacy organizations would argue for original-public-meaning historical evidence and founding-era individual practice, but the collective-right reading denies the premise of their claim. They are structurally excluded because the reading does not recognize individual arms-bearing outside militia as a protected right.
% DISAPPEARANCE_RATIONALE: If the collective-right reading disappeared and were replaced by the individual-right reading (as partially happened with DC v. Heller, 2008), state gun-control regimes would face constitutional scrutiny and many existing regulations would require new justification or would be struck down. The landscape of permissible state regulation would contract significantly. Conversely, if the collective-right reading remains in force, states retain broad regulatory latitude and gun-control regimes persist. The constraint's disappearance would require courts to overturn the interpretation, which would substantially rearrange the regulatory and constitutional landscape.
% FOUNDING_PROBLEM: How to reserve to the states the power to maintain militia forces as an institutional check on federal military monopoly, while permitting states to regulate individual arms possession to protect public safety and order, without creating an individual constitutional right that constrains state regulatory authority.
% FOUNDING_PROBLEM_CORROBORATION: Scholars defending the collective-right reading (Saul Cornell, Carl Bogus) cite founding-era militia law and state constitutions to argue the founding problem was what I stated above — that the founding generation understood the right as protecting militia authority, not individual ownership. However, scholars defending the individual-right reading (Randy Barnett, Eugene Volokh) cite competing historical evidence (founding-era arms possession by individuals, militia participation by armed citizens, state-constitution language protecting individual bearing of arms) to argue that founding-era practice included individual arms ownership. The scholarly disagreement is genuine and unresolved; no neutral source outside both camps provides authoritative corroboration of the founding generation's intent. The historical record is contested.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.15) because the collective-right reading does not impose extraction on any seat — it reallocates rights-holding from individual to state level. States gain regulatory authority, not the distribution of rents. Militia members and public safety entities benefit without paying; individual gun owners outside militia lose access but are not paying a transfer — they are denied a claimed right. Suppression is MODERATE-LOW (0.22) because the reading depends on judicial adoption and acceptance of the historical/textual argument, not on active coercion of those who disagree with it — gun-rights advocates meet substantial resistance and maintain counter-arguments. Theater is MODERATE (0.28) at present: the militia framing has become less functional (state militias are now integrated with federal command structures; the check-on-federal-power rationale is partially theatrical) while the regulatory function has become primary. Over the interval 1791–2026, extractiveness rose modestly (as gun-control regimes developed) and theater rose sharply (1975–2008) before declining as the individual-right reading gained ground post-2008, reducing the performance burden on the collective-right defense. The shared time grid ensures all three metrics are authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The state-government and federal-court seats experience the constraint as a coordination mechanism protecting federalism and militia authority — low-extraction beneficiary positions. Individual gun owners and gun-rights advocates experience the constraint as a denial of claimed rights — high-directionality target positions. Public safety constituencies experience it as enabling their regulatory goals — beneficiary positions. The engine computes these divergences from power, exit_options, and beneficiary/victim declarations; the authored metrics describe the structure from the reading's own epistemic position (states are coordinators, individuals are not rights-holders), not from a neutral standpoint. This is the reading-relative ε: what the collective-right reading's own framework asserts about the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are structural beneficiaries (d near 0.0): they hold the right under this reading and retain regulatory authority. Individual gun owners outside militia are targets (d near 1.0): they claim a right the reading denies, and their exit options are constrained by the constitutional interpretation itself — they cannot escape the reading by choosing to leave. Federal courts are analytical seats (d = 0.5): they interpret but do not collect from the constraint. Gun-rights advocates are excluded (d approaches 1.0 in asymmetry): their claim is foreclosed, and they have no power to alter the reading except through constitutional amendment or successful counter-litigation. The directionality structure is driven by the assignment of rights-holding: if you hold the right, d is low; if you are denied it, d is high. No overrides are required — the structural data (state beneficiary, individual victim/payer of lost claim) maps cleanly to directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserving state militia as a check on federal power) was live in 1791 but has become substantially dead: state militias are now integrated into federal command structures and the federal government's military superiority is absolute. The constraint persists despite the dissolution of its founding justification. However, the constraint does NOT fit the piton pattern because there is an active beneficiary (state governments and regulatory authorities) that maintains it and derives benefit from it — not diffuse costs with no capturer. The reading remains theoretically functional (it frames regulatory authority as legitimate) even if the militia-check function has atrophied. This is an instance of mandatrophy (function obsolescence) but not a piton (which requires diffuse costs and no concentrated benefit). A true piton would be sustained purely by theater and inertia; this constraint has an institutional beneficiary still operating it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_scope,
    'Does the prefatory clause ''A well regulated Militia, being necessary to the security of a free State'' limit the operative clause''s scope to militia-related bearing of arms, or does it merely provide context without limiting scope?',
    'Linguistic analysis of parallel founding-era texts and state constitutions; grammatical principles of prefatory-operative clause relationship; founding-era and subsequent historical practice of arms bearing outside militia service.',
    'If the prefatory clause limits scope, the collective-right reading is reinforced and extractiveness remains low. If the clause provides context only, the operative clause (''the right of the people to keep and bear Arms'') may be interpreted as broader, supporting the individual-right reading and raising extractiveness. This is the pivot point between the collective and individual readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_clause_scope, empirical, 'Whether the prefatory militia language limits the operative clause or provides context.').

omega_variable(
    historical_individual_arms_practice,
    'Did the founding generation understand and practice an individual right to keep and bear arms outside organized militia service, or did they understand the right as purely militia-related?',
    'Historical evidence of civilian arms possession, use, and regulation in founding-era America; state constitutions and bills of rights; militia laws and their application; founding-era commentary and correspondence.',
    'Evidence of widespread founding-era individual arms practice outside militia would undermine the collective-right reading''s claim to original understanding and support the individual-right reading. Evidence that founding-era arms bearing was militia-restricted would support the collective-right reading. This is the empirical foundation of the interpretive dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_individual_arms_practice, empirical, 'Whether founding-era practice included individual arms ownership independent of militia service.').

omega_variable(
    militia_integration_and_state_authority,
    'Given that state militias are now integrated into federal command structures and no longer function as a plausible check on federal power, does the collective-right reading''s founding rationale still apply, or has the constraint become a pure vehicle for state regulatory authority decoupled from its militia justification?',
    'Examination of state militia integration timeline, federal-state command relationships, and judicial reasoning in cases sustaining state regulations. Assessment of whether courts continue to cite militia-check rationale or have shifted to general state-regulatory-authority rationale.',
    'If the constraint''s function has entirely shifted from militia-check to regulatory vehicle, the theater ratio becomes higher and the mandatrophy claim strengthens. This would suggest the reading is sustained by inertia and institutional interests rather than by its original structural function. It affects the classification pressure toward piton, though the presence of active beneficiaries (state governments) prevents full piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_integration_and_state_authority, empirical, 'Whether the militia-check function remains operative or has been superseded by state-regulatory interests.').

omega_variable(
    collective_right_definition_ambiguity,
    'What counts as ''the people'' in the context of a collective right? Is it the states as entities, the citizenry-at-large organized as militia, or state governments acting through their legislatures?',
    'Comparative analysis of how founding-era legal language used ''the people'' (individual subjects or collective bodies); examination of how courts have defined the rights-holder when adopting the collective-right reading; review of state constitutions with explicit militia language.',
    'If ''the people'' means the citizenry-at-large (supporting civic-republican reading), then regulation of individual arms practice may face constitutional constraint even under collective-right framework. If ''the people'' means states or state governments, the regulatory discretion is widest. This ambiguity affects whether the collective-right reading actually forecloses individual claims or merely reallocates the burden of proof.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_right_definition_ambiguity, conceptual, 'Definitional uncertainty in what ''the people'' refers to in collective-right framing.').

omega_variable(
    counter_reading_credibility,
    'Is the credibility and judicial adoption of the individual-right reading (as established in DC v. Heller, 2008) a permanent shift or a potentially temporary inflection that could reverse with changes in court composition?',
    'Monitoring of subsequent Supreme Court decisions on Second Amendment scope; analysis of lower-court decisions; assessment of whether the Heller holding is extended or narrowed; observation of court composition changes and their effects on precedent interpretation.',
    'If the individual-right reading becomes deeply entrenched in precedent and public understanding, the collective-right reading''s authority is permanently diminished; extractiveness could rise as states face new constitutional constraints. If the individual-right reading is narrowed or reversed, the collective-right reading could regain institutional authority. This affects the terminal attractor for the constraint''s evolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_reading_credibility, empirical, 'Whether the post-Heller individual-right reading represents a stable doctrinal shift or a potentially reversible inflection.').

omega_variable(
    committer_field_competing_readings,
    'The collective-right reading is one of three readings of the contested Second Amendment kernel. Do the competing readings (individual-right, civic-republican) logically foreclose the collective-right reading, or do they merely represent different authoritative framings of the same text?',
    'Analysis of the logical structure of each reading''s core premise. If the individual-right reading asserts ''the right exists independent of militia organization,'' does that directly contradict the collective-right assertion ''the right protects militia authority''? Or can both be held by different parties without logical contradiction?',
    'If the readings truly foreclose each other (one true, others impossible), the constraint''s classification depends on which reading is judicially or socially adopted. If they coexist (different legitimate framings of ambiguous text), the constraint''s type may reflect whichever reading dominates current doctrine, with the possibility of sudden reclassification if authority shifts. The coexistence framing supports the omega-variable approach; the foreclosure framing suggests the readings belong in a constraint family with strong network coupling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_field_competing_readings, conceptual, 'Whether the sibling readings foreclose each other or coexist as legitimate interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__collective_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement_basis(seco_tr_t1791, projected).
narrative_ontology:measurement(seco_tr_t1870, second_amendment_arms_right__collective_right_reading, theater_ratio, 1870, 0.08).
narrative_ontology:measurement_basis(seco_tr_t1870, projected).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_arms_right__collective_right_reading, theater_ratio, 1934, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1934, observed).
narrative_ontology:measurement(seco_tr_t1975, second_amendment_arms_right__collective_right_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement_basis(seco_tr_t1975, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__collective_right_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_arms_right__collective_right_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.08).
narrative_ontology:measurement_basis(seco_be_t1791, projected).
narrative_ontology:measurement(seco_be_t1870, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1870, 0.12).
narrative_ontology:measurement_basis(seco_be_t1870, projected).
narrative_ontology:measurement(seco_be_t1934, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1934, 0.14).
narrative_ontology:measurement_basis(seco_be_t1934, observed).
narrative_ontology:measurement(seco_be_t1975, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement_basis(seco_be_t1975, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.18).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2026, 0.15).
narrative_ontology:measurement_basis(seco_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement_basis(seco_su_t1791, projected).
narrative_ontology:measurement(seco_su_t1870, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1870, 0.12).
narrative_ontology:measurement_basis(seco_su_t1870, projected).
narrative_ontology:measurement(seco_su_t1934, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1934, 0.18).
narrative_ontology:measurement_basis(seco_su_t1934, observed).
narrative_ontology:measurement(seco_su_t1975, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1975, 0.2).
narrative_ontology:measurement_basis(seco_su_t1975, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2008, 0.32).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(seco_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__collective_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel (second_amendment_arms_right) decomposes into three structurally distinct constraint stories, one per reading. Each story instantiates a different interpretation of the same constitutional text, with different beneficiary/victim structures, different extracted values, and different terminal attractors. The ε-invariance principle requires separate stories: the collective-right reading assigns ε=0.15 (low extraction, state-regulatory legitimacy); the individual-right reading assigns ε much higher (0.60+, individual-claim denial); the civic-republican reading sits intermediate (0.35, emphasis on civic duty). Each reading's ε is measured relative to the standing arrangement under contest — the constraint as that reading frames it — never relative to any sibling's endorsed alternative. These three stories form a constraint family linked by network.affects_constraints and by the kernel_id recorded in cs_structure. The readings do not merge into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
