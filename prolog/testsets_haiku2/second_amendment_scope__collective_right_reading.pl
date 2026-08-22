% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment Collective-Right Reading: State Militia Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   The Second Amendment states: 'A well regulated Militia, being necessary
 *   to the security of a free State, the right of the people to keep and bear
 *   Arms, shall not be infringed.' The collective-right reading interprets
 *   the prefatory clause ('A well regulated Militia') as the limiting
 *   principle: the Amendment protects state authority to maintain militia
 *   forces, not individual gun ownership. Under this reading, states retain
 *   broad regulatory authority over civilian firearms without federal
 *   constitutional constraint. The reading benefits state institutional
 *   actors and preserves police-power authority; it excludes individual gun
 *   owners from constitutional protection. This is ONE READING of the
 *   contested kernel second_amendment_scope; the sibling readings
 *   (individual_right_reading, civic_right_reading) offer structurally
 *   different beneficiary/victim distributions and different ε values.
 *
 * KEY AGENTS:
 *   - state_militia_authority — benefits from constitutional preservation of state militia capacity without individual-right constraints
 *   - state_regulatory_capacity — agenda-setter; enforces gun regulations via police power
 *   - federalism doctrine — benefits from reading that keeps Second Amendment in state domain
 *   - individual_gun_owners — excluded; would dispute the reading
 *   - constitutional_adjudicators — enforce the reading's boundaries in courts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.28).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.41).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment Collective-Right Reading: State Militia Authority").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, 'acea8b6d-5425-477b-9559-5794fb226ad3').
narrative_ontology:cs_kernel_codification('acea8b6d-5425-477b-9559-5794fb226ad3', fixed_text).
narrative_ontology:cs_authority_grounding('acea8b6d-5425-477b-9559-5794fb226ad3', lineage).
narrative_ontology:cs_interpretation_layer_present('acea8b6d-5425-477b-9559-5794fb226ad3').
narrative_ontology:cs_reading_relation('acea8b6d-5425-477b-9559-5794fb226ad3', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('acea8b6d-5425-477b-9559-5794fb226ad3', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('acea8b6d-5425-477b-9559-5794fb226ad3', foundational, prefatory_clause_limiting_principle).
narrative_ontology:cs_axiom_status(prefatory_clause_limiting_principle, holdable).
narrative_ontology:cs_axiom_grounding('acea8b6d-5425-477b-9559-5794fb226ad3', prefatory_clause_limiting_principle, deontological).
narrative_ontology:cs_axiom('acea8b6d-5425-477b-9559-5794fb226ad3', foundational, federalism_militia_primacy).
narrative_ontology:cs_axiom_status(federalism_militia_primacy, holdable).
narrative_ontology:cs_axiom_grounding('acea8b6d-5425-477b-9559-5794fb226ad3', federalism_militia_primacy, deontological).
narrative_ontology:cs_reference_frame('acea8b6d-5425-477b-9559-5794fb226ad3', founding_militia_check_doctrine).
narrative_ontology:cs_drift_state('acea8b6d-5425-477b-9559-5794fb226ad3', contemporary_gun_policy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('acea8b6d-5425-477b-9559-5794fb226ad3', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_militia_authority).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_regulatory_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, federalism_doctrine_institutional_seats).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, public_safety_constituency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States retain constitutional authority to establish, train, and regulate militia forces (now organized as National Guards) without individual gun ownership becoming a constitutional entitlement. Under this reading, the Second Amendment secures state capacity to field armed forces, not individual armament. States benefit from broad regulatory authority over civilian firearms without federal constitutional constraint, since the right is understood as collective and state-exercised.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_militia_authority, beneficiary,
    institutional, generational, analytical, national).

% State legislatures and executives enforce firearms regulations (licensing, permitting, restriction by category, carry prohibitions) as a police power matter, answerable to their constituents and state constitutional provisions, not constrained by an individual Second Amendment right. Under the collective reading, the Second Amendment does not limit state authority to regulate private gun ownership in the civilian sphere.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_regulatory_capacity, agenda_setter,
    institutional, generational, analytical, national).

% Federal courts and constitutional scholars adhering to federalism principles benefit from a reading that keeps the Second Amendment's force in the state militia domain, preserving state regulatory space and avoiding a federal fundamental right that would preempt state legislatures' judgment on public safety.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federalism_doctrine_institutional_seats, beneficiary,
    institutional, generational, analytical, national).

% Under this reading, individual gun owners have no constitutional claim against state regulations; their interest in unrestricted access depends on state statutory grant, not constitutional right. They are excluded from the constraint's beneficiary structure and would argue for a different reading that protects their ownership interest as a constitutional matter.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_gun_owners, excluded,
    organized, biographical, mobile, national).

% Gun-rights organizations actively dispute this reading and advocate for the individual-right reading. They are structurally excluded from the beneficiary set because this reading does not vindicate their core constitutional claim. Their disagreement is not a compliance cost but a contestation of which reading is correct.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, pro_gun_rights_advocacy, excluded,
    organized, generational, mobile, national).

% Citizens and public health advocates who support gun regulations benefit from a reading that removes federal constitutional obstacles to state legislative action. They can advocate for stronger restrictions without defending against a federal individual-right constraint, though their actual voice in outcomes depends on state-level political power.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, public_safety_constituency, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__collective_right_reading, public_safety_constituency, observer).

% Federal courts and the Supreme Court adjudicate the meaning of the Second Amendment. Under the collective-right reading, courts enforce the constraint by scrutinizing state militia regulations tightly (protecting the state power to maintain forces) while permitting extensive civilian gun regulation (because no individual fundamental right exists). Courts are the primary enforcement machinery of this reading's boundary.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_adjudicators, agenda_setter,
    institutional, generational, analytical, national).

% Constitutional historians and legal scholars engage in evidence-based interpretation of the Founding Era and the Amendment's text. They provide external corroboration (or dispute) of the reading's historical foundation. This seat produces the scholarly consensus that either supports or undermines the collective-right claim.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, historical_scholarship_community, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__collective_right_reading, state_regulatory_capacity).
narrative_ontology:fixing_cost_class(second_amendment_scope__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures state capacity to maintain organized militia forces (now National Guards) as the constitutional guarantee; prevents federal courts from creating an individual gun-ownership right that would fragment state police power and undermine state control of armed forces.
% TRANSFER_FUNCTION: Transfers regulatory authority from individual gun owners (who would have rights) to states (which regulate guns as a police power); moves constitutional protection from individual access to collective state militia function.
% ABSENT_VOICES: Individual gun owners and pro-gun-rights organizations would object loudly and do so in every forum available — they dispute the reading's core claim. They are kept out not physically but by the reading's own epistemic premise (the right is collective, not individual). Their exclusion is structural to the reading itself, not a failure of process.
% DISAPPEARANCE_RATIONALE: If this reading lost judicial authority and were replaced by the individual-right reading, constitutional law would reorganize: federal courts would apply strict scrutiny to state gun regulations; many existing laws (background checks, assault-weapon bans, carry restrictions) would face constitutional challenge; and state regulatory authority would shrink. The foundational structure of Second Amendment jurisprudence would flip.
% FOUNDING_PROBLEM: The Framers feared standing federal armies and sought to preserve state militia capacity as a constitutional check on federal military power. The Second Amendment's prefatory militia clause encodes that original anxiety: states needed the power to arm and train forces independent of federal control.
% FOUNDING_PROBLEM_CORROBORATION: Historians specializing in the Founding Era (e.g., Garry Wills, Michael Bellesiles on the militia tradition; critical response from David Kopel and others) debate whether the founding problem was federal standing-army anxiety or personal-defense concern. The scholarly consensus shifted toward the individual-right reading in the 1990s–2000s (Cato Unbound, Saint George Tucker commentary tradition), but disagreement persists. Corroboration from OUTSIDE the collective-right beneficiary set is mixed: pro-gun historians argue the founding problem was always about individual ownership; anti-gun scholars note that late-20th-century gun-violence rates are a MODERN problem the Framers never faced, so founding-problem status is itself historically contingent.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.28 at interval end) because the reading does not extract value from individual gun owners — it simply denies them constitutional protection and leaves regulation to state discretion. There is no direct transfer or coercion; the reading operates by exclusion (gun ownership is not a constitutional right) rather than by active taking. Suppression is moderate (0.41) because the reading must actively enforce the boundary: courts must reject individual-right claims, state regulators must operate without federal constitutional constraints, and the reading's maintenance requires continuous assertion against high resistance from gun-rights advocates (0.72). Theater is LOW-to-moderate (0.22): the militia function is real (states do maintain National Guards), but the reading's enforcement activity is increasingly about preserving the power to regulate civilian guns, not about militia maintenance itself — hence the theater ratio rises as the interval progresses (0.08 → 0.22), indicating growing gap between stated militia function and actual regulatory use. Accessibility collapse is moderate (0.65): gun ownership remains possible under state permits, so alternatives haven't collapsed completely, but the reading removes the ultimate exit route (federal constitutional protection), constraining the policy space for gun-owner advocacy. The claim/metric independence is preserved here: the constraint is CLAIMED as rope (genuine state militia coordination), and the metrics honestly describe that claim plus the active enforcement it requires. The engine will compute the type from the metrics and structural data; divergence is the signal.
 *
 * PERSPECTIVAL GAP:
 *   The state institutional seats perceive the constraint as rope: genuine coordination problem (maintaining militia capacity) solved by a constitutional principle (the state militia right protected from federal interference). Individual gun owners perceive the constraint differently: as snare (state monopoly on armed-force capacity, preventing private ownership, suppressed by exclusion from constitutional protection). The beneficiary and target perspectives are inverted. The STATE seat (institutional, powerful) sees coordination and legitimate authority. The INDIVIDUAL seat (organized but less powerful) sees extraction and illegitimate exclusion. The engine computes these divergent per-seat classifications from the authored directionality (beneficiaries get low d, targets/excluded get high d) and the power atoms (institutional power amplifies the beneficiary slope). This reading is authored for the state institutional perspective; the sibling individual_right_reading is authored from the gun-owner perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   State militia authority and state regulatory capacity are the beneficiaries — they hold low d (around 0.15–0.25) because the reading enhances their power and removes federal constitutional constraints. Constitutional adjudicators and federalism doctrine benefit from preserving state authority, so they also sit in the low-d end (0.20–0.35). Individual gun owners are excluded from constitutional protection; while they are not named in victims (no explicit transfer), they bear costs through inability to challenge state regulation, so their d is higher (0.55–0.70 as organized actors with constrained but mobile exit options). The public-safety constituency benefits from unobstructed state regulation, so they sit in the low-d beneficiary zone (0.20–0.30). Gun-rights advocacy is structurally opposed to the reading; they sit high-d (0.70–0.85) as organized actors whose interests are excluded. No directionality overrides are needed: the structural derivation from beneficiary/victim + power + exit accurately captures the seats' relationships to this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state militia capacity as constitutional check on federal standing armies) is DEAD in its original form: modern federal armies are vastly larger and more powerful than state militias, and the constitutional check has proven ineffective or irrelevant. States now maintain National Guards as primarily federal-controlled reserve forces, not as independent armed forces checking federal power. The constraint's mandate (preserve state militia capacity as a constitutional principle) has outlived its function: state militia is no longer a viable constitutional check, and modern gun-violence concerns are a DIFFERENT problem than Founding-Era standing-army anxiety. Under the collective-right reading, the constraint persists because it serves modern state regulatory interests (preserving police power over guns), not because it solves the original problem. This is a MANDATROPHY FLAG: the founding problem is dead, yet the constraint persists, now vindicating different (federalism and regulatory power) propositions than the original. The theater ratio rising from 0.08 to 0.22 across the interval documents the increasing gap between militia-maintenance rhetoric and actual regulatory enforcement, a classic mandatrophy signature. A constraint whose mandate has died but whose institutional beneficiaries persist is a candidate for reclassification from rope (genuine coordination) to snare (institutional extraction dressed as settled principle).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_vs_historical_fact,
    'Is the collective-right reading a coherent historical interpretation of the Second Amendment''s original meaning, or a retroactively selected doctrine that benefits state regulatory power?',
    'Comparative historical scholarship: did Founding-Era usage, state militia traditions, and contemporary understanding support a state-authority reading, or does the evidence point toward individual protection? Peer-review consensus in constitutional history.',
    'If the reading is historically sound, it has legitimate authority grounded in fidelity to the kernel text. If the reading is a post-hoc doctrine developed to serve modern policy goals, it is a false-authority reading that benefits institutional seats (states) under the cover of originalism. This affects the constraint''s classification: a historically grounded reading is closer to rope (genuine coordination principle); a doctrine selected for policy is closer to snare (institutional extraction dressed as law).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_vs_historical_fact, empirical, 'Whether the collective-right reading reflects historical original meaning or modern institutional retrofitting.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the collective-right reading logically foreclose the individual-right reading, or do they represent genuinely live alternatives that different parties can hold simultaneously?',
    'Semantic analysis: can a coherent constitutional framework recognize BOTH a state militia power (collective right) AND an individual ownership right as distinct constitutional claims? Or does one axiom necessarily exclude the other?',
    'If the readings foreclose each other, they are incompatible positions and one will eventually dominate via adjudication or amendment. If they coexist, both remain live and the constraint is structurally embedded in an ongoing contest. Foreclusion would suggest the constraint is part of a zero-sum competition for constitutional authority; coexistence would suggest it is a stable (if contested) alternative framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether collective-right and individual-right readings are logically foreclosing or coexisting alternatives.').

omega_variable(
    suppression_mechanism_externalization,
    'How much of the constraint''s measured suppression (0.41) is the reading ITSELF producing (by framing gun ownership as a non-right, enabling state regulation), versus how much is pre-existing state regulatory power that would exist regardless of this reading?',
    'Counterfactual: if the individual-right reading won and courts struck down most state gun restrictions, how much state authority would collapse? If states retained significant regulatory power (licensing, permitting, dangerous-person exclusions), then suppression is largely structural; if state authority crumbles, then the reading contributes substantially to suppression.',
    'High externalization (suppression mostly structural, reading adds little) suggests the reading is capturing an existing power structure and legitimating it, rather than creating new extraction. Low externalization (reading produces the suppression by reframing rights) suggests the reading is actively constructing the constraint. This affects whether the constraint is primarily descriptive (capturing what''s there) or productive (creating what it describes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_externalization, empirical, 'Whether suppression is pre-existing state power or produced by the reading itself.').

omega_variable(
    kernel_authority_lineage_authenticity,
    'Is the Second Amendment''s authority grounded in an unbroken interpretive lineage (common law, Founding principle, precedent), or has the ''collective right'' reading been repeatedly constructed and reconstructed to serve different institutional agendas across eras?',
    'Genealogical: trace the collective-right reading from the Founding through Marshall, through the 20th century, through modern doctrine. Is there continuity, or do new readings emerge when institutional power calculi shift? Historical sociology of constitutional doctrine.',
    'If lineage is authentic and continuous, the reading has structural authority as a faithful transmission of the kernel''s meaning. If the reading is repeatedly reconstructed, the authority is more fragile and the constraint''s persistence depends more on institutional enforcement (higher suppression) than on genuine principle. This maps to the interpretation_layer_present field: a continuous lineage implies functioning interpretation; episodic reconstruction implies the lineage is broken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authority_lineage_authenticity, empirical, 'Whether the collective-right reading has continuous lineage or repeated reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__collective_right_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__collective_right_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__collective_right_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__collective_right_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__collective_right_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__collective_right_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__collective_right_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__collective_right_reading, base_extractiveness, 30, 0.27).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__collective_right_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__collective_right_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__collective_right_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__collective_right_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__collective_right_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__collective_right_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__collective_right_reading, suppression_requirement, 50, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__collective_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, state_gun_regulation__collective_authority).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, federal_militia_clause__dormancy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested constitutional kernel. The Second Amendment's text can be read to protect state militia authority (this reading) or individual gun ownership (sibling reading individual_right_reading) or a hybrid (sibling reading civic_right_reading). Each reading has different structural properties — different ε, different beneficiary sets, different types — because each reading answers the question 'who does the Second Amendment protect?' differently. The kernel itself is stable (the text does not change); the readings change which actors count as beneficiaries. Network links to siblings and to downstream constraints (state gun regulation, militia clause dormancy) document the constraint family's structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
