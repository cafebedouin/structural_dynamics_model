% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Proportionate Border Control Under Human Rights Constraints
 *   domain: political/legal
 *
 * SUMMARY:
 *   The qualified sovereignty reading of border authority asserts that states
 *   retain legitimate control over territorial entry and membership, but that
 *   authority is not absolute—it must be exercised proportionately to genuine
 *   state interests (security, resource capacity, public order) and
 *   consistently with international human rights obligations. This reading
 *   occupies the middle ground between two sibling readings:
 *   sovereignty_primary (unqualified state authority) and freedom_primary
 *   (movement as fundamental right that borders violate). The qualified
 *   sovereignty reading is instantiated in international human rights
 *   treaties, court judgments, and diplomatic practice, but its enforcement
 *   remains contested because the proportionality standard is ambiguous and
 *   state interest is self-assessed. The constraint exhibits substantial
 *   extraction (0.68) because the proportionality requirement operates
 *   primarily as post-hoc review, not pre-entry veto, and the burden of proof
 *   at each stage shifts depending on who is adjudicating.
 *
 * KEY AGENTS:
 *   - state_apparatus: Sets border policy, conducts enforcement, claims proportionality internally—d near 0.0 (full beneficiary)
 *   - excluded_migrants: Denied entry by state determination, no seat in proportionality adjudication—d near 1.0 (full target)
 *   - asylum_seekers: Identity-locked in state assessment process, constrained exit—d near 0.95 (near-full target)
 *   - citizen_constituency: Benefit from membership security, bear diffuse costs—d near 0.40 (net beneficiary)
 *   - security_establishment: Administer enforcement, benefit from resource allocation—d near 0.15 (beneficiary)
 *   - human_rights_bodies: Observer seats, review but do not enforce—d near 0.50 (symmetric)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.68).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.71).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Proportionate Border Control Under Human Rights Constraints").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political/legal").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '9828d473-5e4b-41b2-adfe-6d0f1494bf4d').
narrative_ontology:cs_kernel_codification('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', fixed_text).
narrative_ontology:cs_authority_grounding('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', lineage).
narrative_ontology:cs_interpretation_layer_present('9828d473-5e4b-41b2-adfe-6d0f1494bf4d').
narrative_ontology:cs_reading_relation('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_reading_relation('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', foundational, proportionality_principle_binding).
narrative_ontology:cs_axiom_status(proportionality_principle_binding, holdable).
narrative_ontology:cs_axiom_grounding('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', proportionality_principle_binding, deontological).
narrative_ontology:cs_axiom('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', foundational, human_rights_obligations_justify_constraint).
narrative_ontology:cs_axiom_status(human_rights_obligations_justify_constraint, holdable).
narrative_ontology:cs_axiom_grounding('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', human_rights_obligations_justify_constraint, conventional).
narrative_ontology:cs_reference_frame('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', state_authority_qualified_by_rights).
narrative_ontology:cs_drift_state('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', contemporary_security_escalation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9828d473-5e4b-41b2-adfe-6d0f1494bf4d', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, state_apparatus).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, citizen_constituency).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, security_establishment).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_persons).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, border_adjacent_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, displaced_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets border policy, defines what constitutes legitimate state interest, conducts the enforcement operations (inspection, interdiction, detention). Claims authority to exclude non-citizens as an exercise of sovereignty; claims proportionality review internally. Bears no direct cost of exclusion; extracts political benefit from managed borders and security narrative.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Denied entry or forcibly removed based on state determinations about their admissibility. No seat at the proportionality adjudication; state's assessment of whether exclusion is justified is not subject to their input or appeal. The cost is forfeited opportunity, family separation, return to violence or deprivation. Cannot negotiate or contest the standard applied.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, local).

% Claim protection based on persecution or displacement. State must assess their claims against its own security and resource judgments. Identity-locked: their status as refugees depends on state determination; they cannot unbind themselves from the assessment process without becoming irregular migrants. Detained pending adjudication; limited appeal mechanisms. Subject to removal if assessment is negative.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, asylum_seekers, payer,
    powerless, immediate, identity_locked, regional).

% Persons displaced by conflict or disaster in their own region, seeking temporary or permanent refuge across borders. Carry some benefit from the constraint (orderly processing rather than chaos, eventual settlement for some) but bear the cost of exclusion and prolonged uncertainty in camps or informal settlements. Their exit—moving without state approval—carries legal and safety risks.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_persons, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, displaced_persons, beneficiary).

% Members of the state's political community benefit from borders as markers of membership, from labor market protections where admission is selective, and from state capacity to manage admission. They benefit from the constraint's framing as legitimate and proportionate (not arbitrary). They also face diffuse costs: hospitality obligations, tax expense for adjudication and enforcement, and social friction from exclusion visibility.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, citizen_constituency, beneficiary,
    organized, biographical, mobile, national).

% Border patrol, immigration authorities, security agencies, and enforcement bureaucracies administer the constraint. They benefit from resources allocated to border control, from authority to make exclusion determinations, and from the security narrative that justifies their expansion. They operate within the proportionality framework nominally, but the framework's ambiguity gives them substantial discretion in applying it.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, security_establishment, beneficiary,
    institutional, generational, mobile, national).

% International human rights courts and treaty-body committees review state border practices against proportionality and human rights standards. They lack enforcement power over states but can produce findings that constrain state legitimacy and create reputational cost. They operate at one remove from enforcement but provide the external check on state proportionality assessments.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Populations living near borders who bear friction costs: overflow detention facilities in towns, informal settlements of excluded persons clustered near crossing points, cross-border family separation, travel restrictions on residents of border zones. They are not the primary target of exclusion but experience the enforcement apparatus's presence, resource drain, and social consequences.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, border_adjacent_communities, payer,
    moderate, biographical, constrained, local).

% Alternative interpretations of border authority (freedom_primary and sovereignty_primary readings) are not authoritatively seated in the qualified_sovereignty frame; they would produce different victim sets and different legitimacy conditions but are structurally outside the adjudication process this constraint establishes. They are excluded from the constraint's own logic, not from the broader kernel contest.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, rival_sovereignty_readings, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(border_normative_status__qualified_sovereignty, rival_sovereignty_readings).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, state_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a process for distinguishing legitimate state interest-driven border control from arbitrary or discriminatory exclusion. Creates common ground between sovereignty claims and human rights obligations: states retain authority to control borders, but must justify and proportionately exercise that authority. Solves the problem of how borders can be simultaneously state instruments and human rights-compatible.
% TRANSFER_FUNCTION: Transfers authority over entry/exclusion from absolute state discretion to conditionally-justified state authority, constrained by proportionality review. Transfers the burden of proof onto the state: it must justify exclusions as proportionate to legitimate interests. Moves the cost of non-entry to excluded persons, with some offsetting benefit to them (procedural review, rather than arbitrary exclusion).
% ABSENT_VOICES: Excluded migrants and asylum seekers who cannot access the adjudication process; alternative sovereignty readings (freedom_primary, which would invert the burden of proof on freedom of movement; sovereignty_primary, which would eliminate the proportionality constraint) are not seated as parties and cannot contest the frame itself.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, either sovereignty_primary would reassert (states with unqualified exclusion authority) or freedom_primary would reassert (movement as fundamental right, borders as prima facie violations). The middle ground—qualified authority subject to proportionality review—would collapse. States' legitimacy claims about border control would reorganize around one of the alternative frames.
% FOUNDING_PROBLEM: How can states exercise necessary control over territory and membership (foundational to state capacity) while respecting the human rights of those excluded or displaced (foundational to legitimacy)? The founding problem is the reconciliation of two apparently irreconcilable claims: states must control borders, and individuals have rights that borders affect.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by: (1) International human rights treaties and jurisprudence (ICCPR, ECHR, regional courts) that recognize both state authority and human rights obligations; (2) academic literature on jus cogens norms and border ethics from philosophers and legal scholars not principally advocating for state authority (e.g., Carens, Pogge, Walzer); (3) state practice itself, which invokes proportionality as justification rather than claiming absolute discretion. The problem's status is live because the two claims remain unresolved: states continue to assert authority, and excluded persons continue to assert rights, without complete reconciliation.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects that states exercise substantial control over entry based on criteria (legitimacy, proportionality) that they assess. The constraint's extraction lies in the gap between the nominally neutral proportionality standard and the state's structural ability to define what proportionality means in practice. Suppression (0.71) is high because the constraint persists through active enforcement (detention, interdiction, deportation) and because excluded persons have trapped exit options—they cannot negotiate the proportionality standard from outside the territory. Theater ratio (0.42) reflects that proportionality review is real (courts do overturn exclusions, states do provide some process) but is partial—many exclusions never reach review, and review outcomes skew toward state preference. The measurement series shows extractiveness and suppression rising steeply from t=0 to t=24 (reflecting accumulation of enforcement apparatus and hardening of proportionality standards post-security events), then plateauing from t=24 onward (the constraint stabilizes at a higher extraction level). Theater ratio also rises but more gradually, indicating that performance of proportionality review increases even as the underlying extraction becomes more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   State apparatus and security establishment should compute as beneficiaries of this constraint (they extract authority and resources from its operation). Excluded migrants and asylum seekers should compute as targets (they bear the extraction cost with minimal exit and minimal voice in the proportionality determination). Citizen constituency and human rights bodies occupy middle positions: citizens benefit from the constraint's framing as legitimate but bear diffuse costs; human rights bodies have moderate power but lack enforcement capacity. The engine computes these divergences from power, exit_options, and beneficiary/victim declarations. The state's seat sees coordination; the migrant's seat sees extraction backed by force. That divergence is structural, not a disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: institutional power, analytical exit (the state is the only party whose exit is hypothetical rather than practical—it cannot leave the border control game), benefits from the constraint's authority and resources. Directionality d ≈ 0.0 (full beneficiary). Excluded migrants: powerless, trapped exit (cannot legally cross, cannot appeal effectively, no alternative route), bear the extraction cost directly. Directionality d ≈ 1.0 (full target). Asylum seekers: powerless, identity_locked exit (their status as refugees depends on the state's assessment; they cannot unbind from the assessment process or adopt an alternative identity). Directionality d ≈ 0.95 (near-full target, slightly elevated because the locked identity creates an affirmative obligation on the state to assess rather than simple exclusion). Citizen constituency: organized power, mobile exit (can emigrate or participate in political change), receive both benefits (membership security) and diffuse costs (hospitality obligations, resource expense). Directionality d ≈ 0.35 (slight beneficiary, net positive from the constraint). Security establishment: institutional power, mobile exit (can redirect to other security functions), benefits from resource allocation and authority delegation. Directionality d ≈ 0.15 (beneficiary, but less direct than state apparatus). Human rights bodies: institutional power, analytical exit (cannot enforce, can only review and produce reputational pressure). Directionality d ≈ 0.50 (symmetric—they benefit from the constraint's appeal to human rights standards but bear the cost of reviewing and criticizing it).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids simple mandatrophy by maintaining its founding function: it still reconciles state authority with human rights obligation, still produces proportionality review, still constrains arbitrary exclusion. However, there is a latent mandatrophy risk: if proportionality review becomes purely theatrical (theater_ratio approaches 0.6–0.7) while extraction continues to rise, the constraint's founding function (genuine reconciliation) will have atrophied and the arrangement will persist as pure enforcement backed by a legitimacy claim. The measurement series from t=0 to t=40 shows theater_ratio rising from 0.25 to 0.42, indicating increasing performance burden without corresponding decline in extraction. This is not yet mandatrophy, but it is mandatrophy-adjacent: the constraint is drifting toward a configuration where the proportionality review becomes more visible even as it becomes less substantive. The tangled_rope classification captures this: genuine coordination (reconciling borders and rights) sits alongside genuine asymmetric extraction (unequal voice in proportionality determination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_standard_ambiguity,
    'What constitutes ''proportionate'' exercise of border authority? Is proportionality assessed by international standards (human rights courts), state internal standards, or a hybrid that defers to states on facts while applying external principles?',
    'Analysis of actual adjudication outcomes from human rights courts vs. state administrative reviews. Does proportionality review constrain state determinations (are outcomes reversed), or does it operate as post-hoc rationalization of state decisions?',
    'If proportionality is genuinely external review, the constraint is more rope-like (true coordination) and directionality for powerless agents improves. If proportionality is state-defined, the constraint is more snare-like (extraction backed by legitimacy theater) and directionality for powerless agents worsens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_standard_ambiguity, empirical, 'Whether proportionality review is substantive or performative.').

omega_variable(
    legitimate_state_interest_definition,
    'Which state interests count as ''legitimate'' for proportionality purposes? Does security include public anxiety? Does resource capacity include distribution preferences? Does cultural preservation count? Who decides?',
    'Comparative analysis of how different states and courts define legitimate interest. Do human rights bodies constrain state definitions, or do they defer on what is ''legitimate''?',
    'If legitimate interests are narrowly defined and externally constrained, extraction decreases and directionality for powerless agents improves. If states have broad latitude in defining legitimate interests, extraction increases and the constraint drifts toward sovereignty_primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_state_interest_definition, conceptual, 'The scope of what counts as a legitimate state interest in border control.').

omega_variable(
    identity_lock_stability_in_asylum_status,
    'For asylum seekers classified as identity_locked (their status as refugees depends on state assessment), does the locked status persist if they are denied asylum and removed? Or does their identity unbind, allowing them to claim a different status (migrant, temporary resident, irregular)?',
    'Post-removal trajectory analysis: do denied asylum seekers remain legal non-persons with respect to re-entry, or do they reclassify into other entry categories? Do they retain de facto access to reapplication?',
    'If identity remains locked post-removal (they stay designated as rejected asylum seekers), suppression remains high and directionality for asylum seekers stays near 1.0 (full target). If identity unbinds (they can reapply or adopt alternative status), suppression decreases slightly and directionality improves marginally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_stability_in_asylum_status, empirical, 'Whether asylum-seeker identity is locked only during assessment or locked indefinitely post-removal.').

omega_variable(
    sovereignty_primary_vs_qualified_sovereignty_boundary,
    'Is there a material difference between qualified_sovereignty (proportionate authority constrained by human rights) and sovereignty_primary (foundational authority to exclude)? Or does qualified_sovereignty collapse into sovereignty_primary when states define proportionality and legitimate interest?',
    'Structural analysis: does the proportionality requirement change state behavior outcomes, or does it merely provide a template states fill in their own favor? If outcomes are the same under both readings, they are not distinct constraints.',
    'If qualified_sovereignty produces different outcomes than sovereignty_primary (fewer exclusions, more process, stronger appeal mechanisms), it is a materially distinct constraint and the reading is robust. If outcomes converge, the readings are functionally identical and qualified_sovereignty is theater—mandatrophy territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_primary_vs_qualified_sovereignty_boundary, conceptual, 'Whether qualified sovereignty is structurally distinct from absolute sovereignty.').

omega_variable(
    theater_ratio_causality,
    'Is the rising theater_ratio (from 0.25 to 0.42 over the interval) caused by increasing demand for procedural legitimacy (states invest in review machinery to maintain authority), or by declining belief in proportionality review''s substance (review is performative)?',
    'Qualitative analysis of state rhetoric and legal architecture: are states adding proportionality machinery because they believe in it, or because it buffers them against criticism? Are courts expanding review, or just ceremony?',
    'If states invest in review because they believe in proportionality, theater_ratio rise indicates healthier constraint operation and signals movement toward rope-like coordination. If theater_ratio rise indicates performative investment without substance, it signals drift toward snare (extraction backed by ceremony).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_ratio_causality, conceptual, 'Whether theater_ratio increase reflects genuine belief in proportionality or increasing need for legitimacy cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__qualified_sovereignty, theater_ratio, 8, 0.3).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__qualified_sovereignty, theater_ratio, 16, 0.36).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__qualified_sovereignty, theater_ratio, 24, 0.41).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__qualified_sovereignty, theater_ratio, 32, 0.42).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__qualified_sovereignty, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t8, border_normative_status__qualified_sovereignty, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(bord_be_t16, border_normative_status__qualified_sovereignty, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(bord_be_t24, border_normative_status__qualified_sovereignty, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(bord_be_t32, border_normative_status__qualified_sovereignty, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(bord_be_t40, border_normative_status__qualified_sovereignty, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t8, border_normative_status__qualified_sovereignty, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(bord_su_t16, border_normative_status__qualified_sovereignty, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(bord_su_t24, border_normative_status__qualified_sovereignty, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(bord_su_t32, border_normative_status__qualified_sovereignty, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(bord_su_t40, border_normative_status__qualified_sovereignty, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__qualified_sovereignty, 0.18).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, asylum_adjudication_standards).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, state_security_exception_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_normative_status kernel. The sibling readings (freedom_primary, sovereignty_primary) are separate constraint stories with different ε values, different beneficiary/victim structures, and different classifications. All three share the same kernel text but instantiate different structural relationships to it. The qualified_sovereignty reading creates downstream pressure on both siblings by introducing proportionality review and external constraint as binding conditions; it influences rather than forecloses them. See kernel_context in commentary for the full structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
