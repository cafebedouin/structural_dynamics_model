% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment: Originalist Civic Virtue Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint story models the originalist civic virtue reading of the
 *   Second Amendment: the founding-era militia understood as the universal
 *   armed citizenry, with the right protecting the citizen-soldier capacity
 *   as a constitutive element of republican citizenship. The reading treats
 *   the prefatory militia clause not as a limitation but as the key to the
 *   operative clause's scope — the right exists to secure the civic virtue of
 *   an armed citizenry capable of self-governance through collective defense.
 *   The beneficiary is the citizenry qua political community; no specific
 *   victim set is declared because the constraint is framed as a rightful
 *   limitation on government, not an extraction from a victim class. This is
 *   one of three declared readings of the second_amendment_text kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.22).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.15).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment: Originalist Civic Virtue Reading").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__originalist_civic_virtue_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, '3fa42b0c-bc9b-4921-a22e-7a5c80ead974').
narrative_ontology:cs_kernel_codification('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', fixed_text).
narrative_ontology:cs_authority_grounding('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', lineage).
narrative_ontology:cs_interpretation_layer_present('3fa42b0c-bc9b-4921-a22e-7a5c80ead974').
narrative_ontology:cs_reading_relation('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', foundational, armed_citizenry_as_civic_virtue).
narrative_ontology:cs_axiom_status(armed_citizenry_as_civic_virtue, holdable).
narrative_ontology:cs_axiom_grounding('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', armed_citizenry_as_civic_virtue, deontological).
narrative_ontology:cs_axiom('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', secondary, militia_universality_presumption).
narrative_ontology:cs_axiom_status(militia_universality_presumption, holdable).
narrative_ontology:cs_axiom_grounding('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', militia_universality_presumption, empirically_contingent).
narrative_ontology:cs_reference_frame('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', founding_civic_republican_order).
narrative_ontology:cs_drift_state('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', contemporary_incorporation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3fa42b0c-bc9b-4921-a22e-7a5c80ead974', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizenry_political_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, current_government).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republican_virtue).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, universal_militia_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, armed_citizenship_as_political_participation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The political community as a collective body that maintains its republican character through the universal capacity for armed civic defense. The constraint protects their structural ability to function as a militia rather than depend on a standing army. Exit from this identity is constrained by the constitutional frame itself — one cannot opt out of the civic republican premise without leaving the polity.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, citizenry_political_community, beneficiary,
    organized, generational, constrained, national).

% The historical authors and ratifiers who embedded the civic republican militia understanding into the constitutional text. Their intent sets the agenda for this reading's interpretation. They do not bear costs or collect benefits in the present; they are the authoritative source of the constraint's design logic.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, constitutional_framers, agenda_setter,
    institutional, civilizational, analytical, national).

% Federal and state governments whose regulatory authority over arms is limited by this reading. They bear the cost of forgone regulatory options (the 'transfer' of regulatory authority to constitutional protection). They also administer the legal system that enforces the constraint. In this reading, this constraint is not extraction from government but rightful constitutional limitation — yet structurally they are the seat from which regulatory power is withheld.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, current_government, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__originalist_civic_virtue_reading, current_government, agenda_setter).

% Courts that interpret and enforce the constraint against legislative encroachment. They are the active enforcement mechanism. Their institutional legitimacy in this reading depends on fidelity to the founding-era civic republican understanding.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Actors who would prioritize collective security through comprehensive firearms regulation. They are structurally excluded from the civic republican frame this reading constructs — their preferred regulatory framework is treated as constitutionally foreclosed by the civic virtue premise. They would argue the founding problem is obsolete and the constraint now produces net harm.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, gun_control_advocates, excluded,
    organized, biographical, mobile, national).

% Academic observers who evaluate the historical fidelity and structural coherence of this reading against competing interpretations. They neither collect benefits nor bear costs from the constraint's operation; they map the interpretive landscape.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, legal_scholars_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the political community as a universal militia for collective defense, solving the collective action problem of civic defense without a standing army by distributing the capacity and obligation for armed defense across the entire citizenry.
% TRANSFER_FUNCTION: Transfers regulatory authority over militia-suitable arms from the legislature to constitutional protection, moving the power to disarm the citizenry from government to a supermajoritarian amendment process. The constraint moves the default from 'government may regulate' to 'government may not infringe' for arms connected to civic militia capacity.
% ABSENT_VOICES: Modern urban populations for whom universal militia service is materially impractical; victims of gun violence who would argue the civic function is obsolete and the constraint enables harm; state and local governments that would regulate firearms for public safety but are barred by this reading's scope. They are excluded because the civic republican frame defines the political community in founding-era terms that do not accommodate these perspectives.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the constitutional barrier to comprehensive disarmament of the citizenry would disappear. Federal and state governments could then monopolize military-grade arms and regulate civilian ownership to near-zero without constitutional obstacle, fundamentally altering the balance between state military power and civic armed capacity that the civic republican structure depends on.
% FOUNDING_PROBLEM: The Founders feared standing armies as instruments of tyranny and needed to ensure the political community retained the capacity for self-defense against both external threats and internal usurpation, without creating a permanent military caste separated from the citizenry.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of Founding debates (Federalist Nos. 28, 29, 46; Anti-Federalist writings; the 1792 Militia Act) corroborate the civic republican framing as the dominant founding-era understanding. Contemporary historians outside the gun rights movement (e.g., Saul Cornell, 'A Well-Regulated Militia'; the amicus brief of historians in District of Columbia v. Heller) attest the founding problem was genuine but dispute its modern vitality given the emergence of a permanent professional military and the National Guard system.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint's primary structure is coordination — it solves the collective action problem of civic defense by constitutionally entrenching the citizenry's armed capacity. Suppression is low (0.15) because the constraint does not actively suppress alternatives; it elevates one coordination mechanism (universal militia) above legislative displacement. Theater ratio is low (0.12) — the civic republican function is genuinely operational in the reading's logic, not performative. Accessibility collapse is moderate (0.38) — alternative defense structures (standing army, selective militia) are constitutionally disadvantaged but not eliminated. Resistance is moderate-high (0.58) — the reading faces sustained challenge from collective security and individual right frameworks. The measurement series shows gradual drift in extractiveness and suppression as the civic republican premise becomes less materially instantiated (professional military, National Guard, urbanization).
 *
 * PERSPECTIVAL GAP:
 *   From the citizenry seat, the constraint is experienced as a Rope — genuine coordination enabling civic virtue. From the current_government seat, the same constraint is experienced as a restriction on legitimate police power — potentially a Snare if the civic function is deemed obsolete. The engine computes this divergence from the structural data. The reading's proponents deny the Snare characterization by asserting the founding problem remains live; critics assert mandatrophy. The perspectival gap is exactly the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry_political_community is the structural beneficiary (d near 0.0) — the constraint subsidizes their civic capacity by constitutionally protecting it. Current_government is the structural payer (d near 1.0) — it bears the cost of forgone regulatory authority. The judiciary sits near symmetric (d ~0.5) — it gains interpretive authority but bears enforcement burden. Constitutional_framers are analytical (d = 0.5 by default) — they set the agenda but do not experience the constraint's operation. Gun_control_advocates are excluded — their exclusion is structural to the reading's frame. The directionality derivation from beneficiary/payer declarations plus power/exit produces the expected pattern: organized beneficiary with constrained exit, institutional payer with constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve the standing army tyranny problem (founding_problem). That problem's status is contested: originalists argue the civic republican logic persists (a disarmed citizenry remains vulnerable to tyranny); living constitutionalists argue the problem is dead (professional military under civilian control, National Guard as organized militia). If the founding problem is dead but the constraint persists with active enforcement, the constraint drifts toward Piton — theatrical maintenance of an atrophied function. The theater_ratio trajectory (rising from 0.05 to 0.12) tracks this drift. The mandatrophy_resolved flag is not set because the status is contested, not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_virtue_vs_constructed_frame,
    'Is the civic republican reading a genuine recovery of the founding-era structural logic, or a constructed frame that serves contemporary gun rights advocacy by displacing the individual right reading''s vulnerability to public safety critique?',
    'Comparative intellectual history: trace the civic virtue reading''s provenance in pre-Heller scholarship vs. its deployment in post-Heller litigation. If the reading''s modern form relies on scholarly work that post-dates the individual right reading''s rise, the construction hypothesis gains weight.',
    'If constructed, the reading''s claimed Rope coordination function is a cover for a Snare-like extraction (blocking regulation that majorities favor). If genuine recovery, the coordination function stands and the mandatrophy question turns on whether the founding problem persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_virtue_vs_constructed_frame, conceptual, 'Whether the civic virtue frame is authentic founding logic or modern strategic construction.').

omega_variable(
    government_as_payer_not_victim,
    'Does the current_government stakeholder genuinely occupy a ''payer'' seat (bearing rightful constitutional costs) or a ''victim'' seat (subject to extraction by an obsolete constraint)?',
    'Normative analysis of constitutional legitimacy: if the constraint''s founding problem is live, government''s constrained authority is a feature not a bug (payer). If the founding problem is dead, the constraint extracts regulatory capacity without returning civic function (victim). The six_questions.founding_problem_status = contested means this omega remains open.',
    'If government is a victim, the constraint reclassifies toward Snare/Tangled Rope from the government seat. If government is a rightful payer, the Rope classification holds across seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(government_as_payer_not_victim, preference, 'Whether the constrained government actor is a rightful payer or an extraction victim.').

omega_variable(
    universal_militia_material_feasibility,
    'Is the universal militia coordination function materially feasible in contemporary conditions (urbanization, professional military, technological complexity of arms), or has the coordination target become a phantom?',
    'Institutional feasibility study: can a universal militia be organized, trained, and equipped under modern conditions without a standing army infrastructure? Historical comparison to Swiss/Israeli models vs. US conditions.',
    'If infeasible, the coordination function is fictive and the constraint''s Rope claim collapses — it becomes a Piton (theatrical maintenance) or Snare (extraction via foreclosed regulation). If feasible, the Rope claim survives the material challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_militia_material_feasibility, empirical, 'Whether the coordination function the constraint purports to serve can actually operate today.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1868, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1934, 0.1).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 1968, 0.11).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1791, 0.08).
narrative_ontology:measurement(seco_be_t1868, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1868, 0.12).
narrative_ontology:measurement(seco_be_t1934, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1934, 0.18).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 1968, 0.2).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1791, 0.05).
narrative_ontology:measurement(seco_su_t1868, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1868, 0.08).
narrative_ontology:measurement(seco_su_t1934, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1934, 0.12).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 1968, 0.14).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2008, 0.15).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_text__originalist_civic_virtue_reading, 0.08).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form a constraint family decomposing the second_amendment_text kernel. The civic virtue reading (this story) claims the right protects citizen-soldier capacity as civic virtue (Rope, low extraction, citizenry beneficiary). The collective_security_reading claims the militia clause authorizes state regulation for collective security (potential Scaffold or Tangled Rope). The individual_right_reading claims personal self-defense as the core protected activity (claims vary: Mountain, Rope, or Tangled Rope depending on regulatory scope). All three share the same kernel_id but instantiate different constraints with different ε, beneficiaries, and structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__originalist_civic_virtue_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
