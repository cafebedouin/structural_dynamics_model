% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention – Restrictive Sovereignty Reading
 *   domain: international_law/migration_governance
 *
 * SUMMARY:
 *   The Refugee Convention (1951) and its Protocol (1967) establish a binding
 *   international obligation to provide asylum to persons with a
 *   'well-founded fear of persecution' for one of five enumerated grounds
 *   (race, religion, nationality, political opinion, particular social
 *   group). The restrictive sovereignty reading interprets the Convention as
 *   a minimum floor—states must not refouler (return) someone to imminent
 *   persecution—but retain maximum discretion in how they define
 *   'persecution,' 'well-founded fear,' and 'particular social group.' Under
 *   this reading, persecution must be individualized and state-based,
 *   generalized violence does not qualify, and 'particular social group'
 *   refers only to immutable characteristics that the state recognizes and
 *   consciously targets. This reading serves as a legitimizing frame for
 *   high-threshold asylum screening and offshore processing (e.g., visa
 *   restrictions, maritime interdiction). The reading's core axiom is that
 *   state sovereignty in admission decisions is the foundational commitment;
 *   humanitarian protection is a constraint on that discretion, not a mandate
 *   that displaces it. The expansive humanitarian reading contests this
 *   framing entirely, holding that the Convention's purpose is unbendable
 *   humanitarian obligation. The procedural integrity reading occupies a
 *   middle position, accepting discretion in outcome but requiring procedural
 *   fairness.
 *
 * KEY AGENTS:
 *   - Sovereign states operating asylum systems (institutional power, controls the reading's application)
 *   - Border control and immigration enforcement apparatus (powerful, enforces the screening gates)
 *   - Asylum seekers from generalized violence, non-state persecution, and non-immutable social groups (powerless, identity-locked by the persecuting state, trapped at the border)
 *   - International humanitarian organizations and refugee advocates (analytical/observer position, document the exclusions)
 *   - UNHCR and international refugee law scholarship (analytical, provide alternative interpretations)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.71).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention – Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '47df3cd5-c7d5-4814-9c7a-fc76126a3651').
narrative_ontology:cs_kernel_codification('47df3cd5-c7d5-4814-9c7a-fc76126a3651', fixed_text).
narrative_ontology:cs_authority_grounding('47df3cd5-c7d5-4814-9c7a-fc76126a3651', extraction).
narrative_ontology:cs_interpretation_layer_present('47df3cd5-c7d5-4814-9c7a-fc76126a3651').
narrative_ontology:cs_reading_relation('47df3cd5-c7d5-4814-9c7a-fc76126a3651', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('47df3cd5-c7d5-4814-9c7a-fc76126a3651', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('47df3cd5-c7d5-4814-9c7a-fc76126a3651', foundational, state_sovereignty_discretion_foundational).
narrative_ontology:cs_axiom_status(state_sovereignty_discretion_foundational, holdable).
narrative_ontology:cs_axiom_grounding('47df3cd5-c7d5-4814-9c7a-fc76126a3651', state_sovereignty_discretion_foundational, deontological).
narrative_ontology:cs_axiom('47df3cd5-c7d5-4814-9c7a-fc76126a3651', foundational, persecution_requires_individual_targeting).
narrative_ontology:cs_axiom_status(persecution_requires_individual_targeting, holdable).
narrative_ontology:cs_axiom_grounding('47df3cd5-c7d5-4814-9c7a-fc76126a3651', persecution_requires_individual_targeting, empirically_contingent).
narrative_ontology:cs_axiom('47df3cd5-c7d5-4814-9c7a-fc76126a3651', secondary, particular_social_group_immutability_requirement).
narrative_ontology:cs_axiom_status(particular_social_group_immutability_requirement, holdable).
narrative_ontology:cs_axiom_grounding('47df3cd5-c7d5-4814-9c7a-fc76126a3651', particular_social_group_immutability_requirement, conventional).
narrative_ontology:cs_reference_frame('47df3cd5-c7d5-4814-9c7a-fc76126a3651', state_discretion_within_humanitarian_minimum).
narrative_ontology:cs_drift_state('47df3cd5-c7d5-4814-9c7a-fc76126a3651', contemporary_asylum_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('47df3cd5-c7d5-4814-9c7a-fc76126a3651', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states_enforcement).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_control_apparatus).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_generalized_violence).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_non_state_persecution).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_non_immutable_social_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Refugee Convention's text and applies it through asylum adjudication, visa policy, and border control. Under the restrictive reading, states argue they are entitled to narrow the scope of 'persecution,' require individualized proof, apply the immutability test, and demand state awareness of group targeting. They administer the asylum screening gates and enforce the restrictions. States collect the benefit of narrow obligations—reduced asylum intake, reduced welfare and housing burdens, reduced political pressure from opposition to immigration.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states_enforcement, agenda_setter,
    institutional, generational, analytical, national).

% Implements the restrictive reading's gatekeeping through visa denial, maritime interdiction, offshore processing, and detention. The reading provides them with clear rules for rejecting claims (generalized violence, non-immutable group, no state awareness). The apparatus benefits from the clarity and from the political protection that the reading offers—rejection decisions can be justified with reference to the Convention itself, framing refusal as legal compliance rather than discretionary cruelty.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_control_apparatus, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__restrictive_sovereignty_reading, border_control_apparatus, beneficiary).

% Flee contexts of generalized violence (civil wars, gang warfare, state collapse) where they face high mortality risk alongside millions of others. Under the restrictive reading, they do not qualify for asylum because their persecution is not individualized—the state is not singling them out, the violence is 'general condition' not 'persecution.' They are barred from legal entry, face visa denial and maritime interception, and are returned to the violence they fled. Their exit is trapped: cannot return safely, cannot legally migrate, cannot stay in the territory they reach.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_generalized_violence, payer,
    powerless, immediate, trapped, global).

% Flee persecution by non-state actors (Islamist militias, cartels, clan-based groups, human traffickers) where the state is absent, complicit, or unable to provide protection. Under the restrictive reading, they do not qualify because persecution requires state involvement—private persecution, however systematic, does not meet the definition. They face the same gatekeeping as generalized violence survivors: visa denial, interdiction, return. The trap is the same.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_non_state_persecution, payer,
    powerless, immediate, trapped, global).

% Flee persecution based on gender (women fleeing domestic violence networks tolerated or enforced by state dysfunction), occupation (journalists, political activists, teachers in authoritarian contexts), clan affiliation (persecution not based on immutable ethnicity but on social organization the state does not 'recognize'), or LGBTQ+ identity in contexts where the state criminalizes but does not officially target (the state-awareness requirement excludes implicit persecution). Under the restrictive reading, these groups do not qualify because they are not immutable characteristics of which the state is consciously aware. They face the same refusal and return.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers_non_immutable_social_groups, payer,
    powerless, immediate, trapped, global).

% Document asylum rejections, displacement crises, and the humanitarian consequences of the restrictive reading. They advocate for broader interpretation, conduct research on persecution patterns, and provide testimony to international bodies and courts. They observe the constraint's operation from outside the state-asylum seeker relation, seeing the exclusion it produces and working to shift it. They lack enforcement power but provide analytical pressure and advocacy.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, humanitarian_organizations, observer,
    moderate, generational, analytical, global).

% Interprets the Convention from outside any single state's interest. UNHCR's Handbook provides guidance that is more expansive than the restrictive sovereignty reading, recognizing generalized violence and non-state persecution as valid grounds. Refugee law scholarship documents the reading's evolution and contests restrictive interpretations. They sit at the analytical seat, providing alternative framings and challenging the reading's legitimacy claim.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr_and_refugee_scholarship, observer,
    institutional, generational, analytical, global).

% Review asylum decisions for compliance with international law, including the Refugee Convention, the Covenant on Civil and Political Rights, and the Convention Against Torture. Some courts (EU Court of Justice, some national supreme courts) have pushed back against the restrictive reading, recognizing generalized violence and non-state persecution. Others defer to state interpretation. They occupy the analytical/reviewing seat with institutional power but no direct control over state asylum policy.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, international_courts_and_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Advocates for the expansive humanitarian reading (broad protection, generalized violence, non-state persecution, flexible 'particular social group'). They are excluded from the restrictive reading's decision-making apparatus—states that adopt the restrictive reading do not incorporate their interpretation into asylum policy. They contest the reading through litigation, advocacy, scholarship, and international pressure, but they lack enforcement power over individual state policies.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, expansive_humanitarian_reading_coalition, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(refugee_convention_text__restrictive_sovereignty_reading, expansive_humanitarian_reading_coalition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states_enforcement).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Refugee Convention solves a coordination problem: preventing states from returning individuals to imminent persecution (refoulement prohibition). Without the Convention, states could discharge unwanted refugees unilaterally; the Convention establishes a shared commitment that persecution is a boundary condition for admission. The restrictive reading narrows this to: preventing return of individuals facing individualized, state-based persecution from immutable groups the state consciously targets.
% TRANSFER_FUNCTION: The constraint moves the burden of protection from individuals and humanitarian organizations to states. Under the expansive reading, this burden is broad (protection from generalized violence, non-state persecution, gender-based persecution). Under the restrictive reading, the burden is narrow (protection from individualized state-targeted persecution of immutable groups). The reading transfers the discretion to narrow asylum obligations from international humanitarian bodies back to individual states.
% ABSENT_VOICES: Asylum seekers from generalized violence, non-state persecution, and gender/occupational persecution are structurally excluded from the decision-making apparatus that interprets and applies the Convention—they are the subjects of the rules, not participants in their formulation. Advocacy organizations and humanitarian bodies contest the reading but lack enforcement power. States that would benefit from expansive interpretation (geographically-adjacent states receiving large refugee flows) have limited voice in how other states interpret their Convention obligations. The restrictive reading's formulation excludes the most vulnerable constituencies from the conversation.
% DISAPPEARANCE_RATIONALE: If the restrictive sovereignty reading disappeared overnight and were replaced by either the expansive humanitarian or the procedural integrity reading, the world would rearrange substantially: asylum admission rates would increase, generalized violence and non-state persecution would become cognizable grounds, processing thresholds would shift, and the composition of asylum seekers admitted to wealthy states would change. The reading's narrow scope is what licenses the high rejection rates and offshore processing strategies currently in place—without it, the constraint's enforcement machinery would need to be rebuilt.
% FOUNDING_PROBLEM: The original founding problem (1951) was the World War II displacement crisis and the statelessness of Holocaust survivors and war refugees. The problem was that states, acting without international constraint, could refouler refugees to genocidal regimes, and there was no binding obligation to prevent it. The Convention was established to make refoulement illegal and to provide a minimum floor of protection.
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian organizations and UNHCR attest the founding problem persists in contemporary form: Syrian civil war (6+ million refugees), Venezuelan collapse (5+ million), Myanmar persecution of Rohingya, gang violence in Central America. These are not solved problems but ongoing humanitarian emergencies. States argue the founding problem (systematic refoulement to death) is solved by the Convention itself and that narrow interpretation is appropriate. Refugee law scholarship is divided: some scholars argue the founding problem has evolved (generalized violence and non-state persecution are now primary sources of displacement, not state persecution); others argue the Convention's text was always narrower than humanitarian advocates claim. The disagreement is over what the founding problem actually is and whether it persists.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because the reading functions to reduce asylum obligations below what humanitarian interpretation would impose, allowing states to classify large categories of vulnerable people (civil-war flee rs, gang-violence survivors, gender-persecuted individuals in non-state contexts) as economic migrants. The constraint extracts state discretion—permits maximum flexibility in refusing claims—while framing that discretion as textually required. Suppression is high (0.71) because the reading is actively maintained through asylum adjudication training, judicial reasoning, and the enforcement machinery of visa denial and border interdiction. A claim that is sufficiently narrow to exclude most claimants requires continuous suppressive work. Theater ratio is moderate (0.38) because part of the constraint's machinery is genuine procedural scrutiny (detecting fraudulent claims, verifying persecution), but an increasing share is purely protective-of-sovereignty gatekeeping (the immutability boundary, the individualization requirement, the state-awareness gloss). The measurement series tracks the drift from 1951 (when the Convention was a genuine humanitarian constraint on state power) to contemporary practice (where the reading's restrictive gates have hardened into screening mechanisms). The time index represents decades of case law, practice evolution, and interpretive hardening.
 *
 * PERSPECTIVAL GAP:
 *   From the state perspective, the restrictive reading is a textual mandate: the Convention says 'persecution' (singular, not generalized violence), 'well-founded fear' (requiring individualized evidence), and 'particular social group' (implying a bounded category, not a mass). States experience the reading as clarification of their legal obligations, not constraint—the text permits them to narrow their scope while staying compliant. From the asylum seeker perspective, the reading functions as a closed door: generalized violence does not qualify (despite destroying your life), non-state persecution does not qualify (despite being the primary form of persecution in failed states and gang-dominated contexts), and your gender, clan, or occupational persecution does not qualify because it does not meet the immutability or state-awareness tests. The seeker experiences the reading as a reinterpretation that strips away protection they believed the Convention provided. This gap is structural and irreducible without changing the reading itself—one seat's clarification is the other seat's exclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   States and their border-control apparatus are clear beneficiaries of this reading (d ≈ 0.1–0.2): it licenses them to narrow their asylum obligations while maintaining textual compliance with the Convention. Asylum seekers—particularly those fleeing generalized violence, non-state persecution, or gender-based persecution not recognized as state-targeted—are the structural targets of the reading's restrictive interpretation (d ≈ 0.8–0.9). They are trapped (exit options = trapped: fleeing persecution, cannot return, cannot legally enter without an asylum grant), identity-locked by the persecuting state (the state's targeting defines their legal status), and face suppression through visa denial, maritime interdiction, and bureaucratic gatekeeping. Humanitarian organizations and international courts occupy analytical positions (d ≈ 0.5), observing the structure and sometimes contesting it through litigation and advocacy. The reading's power concentration (institutional states) combined with the victimhood concentration (powerless asylum seekers) creates the classic asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of the Refugee Convention was the World War II displacement crisis and the statelessness vacuum that followed. The original mandate was to prevent refoulement of persons facing existential persecution. The restrictive sovereignty reading acknowledges the mandate exists (no automatic refoulement) but argues the mandate's scope is narrow and states retain maximum interpretation power within that floor. A classical mandatrophy pattern would be: founding problem (preventing Holocaust-scale atrocities) is solved, but the constraint persists as a narrowing gate. However, the founding problem has NOT fully been solved—generalized violence and non-state persecution are massive contemporary sources of displacement (Syrian civil war, Venezuelan collapse, gang violence in Central America, Rohingya persecution by non-state militias). The reading's extraction mechanism is parasitic on an ongoing humanitarian emergency. The mandatrophy is not complete, but the reading manufactures a narrowing that undercuts the Convention's actual humanitarian purpose. This is a Tangled Rope (coordination function—the Convention genuinely solves the refoulement problem for a narrow class—plus extraction—the reading narrows that class using sovereignty discretion as the framing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    immutability_boundary_ambiguity,
    'What counts as an ''immutable characteristic'' sufficient for ''particular social group'' status under this reading? Where does the boundary lie between immutable traits (sex, ethnicity, disability) and acquired or performative ones (occupation, political opinion, family clan affiliation)?',
    'Systematic review of state practice in asylum adjudication: which group categories are admitted as immutable across different jurisdictions and how do they differ from those rejected as non-immutable?',
    'A narrow immutability boundary maximizes filtering power (excludes clan-based, occupational, political-opinion groups); a broader boundary admits claims the reading''s proponents argue are not cognizable. The boundary is not a natural fact — it is a normative choice embedded in the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_boundary_ambiguity, conceptual, 'The boundary between immutable and non-immutable characteristics is reading-dependent, not empirically fixed.').

omega_variable(
    state_awareness_requirement_circularity,
    'Does ''particular social group'' require that the state be aware of the persecuted group and target it deliberately, or is implicit discrimination sufficient? Does the state''s unawareness of a group''s persecution absolve it from refugee law obligations?',
    'Examine case law on whether systematic but unstated discrimination (e.g., de facto clan targeting by a regime claiming not to recognize clans) meets the ''state awareness'' gate. Compare decisions treating implicit vs. explicit targeting.',
    'If ''state awareness'' means deliberate targeting, the requirement shields states that engage in structural discrimination while maintaining plausible denial. If implicit/systematic targeting counts, the gate closes and more groups become cognizable. This is a core interpretive pivot for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_awareness_requirement_circularity, conceptual, 'Whether ''state awareness'' in the ''particular social group'' criterion refers to explicit, deliberate targeting or includes implicit systematic persecution.').

omega_variable(
    individualized_persecution_vs_generalized_violence,
    'Can ''well-founded fear'' of persecution be triggered by participation in generalized violence (gang warfare, civil conflict) where the applicant is one of many at risk, or must the applicant demonstrate that the state has singled them out individually?',
    'Examine state practice and judicial review: are asylum claims from individuals fleeing gang violence or civil war denied solely on the ground that they were not individually targeted? Do humanitarian emergencies (e.g., Venezuelan displacement) produce mass rejections under this reading''s standard?',
    'A strict individualization requirement screens out large classes of genuinely vulnerable people (civil war, generalized gang violence, state collapse) by classifying them as economic migration or general hardship rather than persecution. This is the empirical fact-pattern where the reading''s extraction mechanism is most visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individualized_persecution_vs_generalized_violence, empirical, 'Whether the requirement for individualized persecution proof excludes people fleeing generalized violence and large-scale humanitarian emergencies.').

omega_variable(
    kernel_reading_contest,
    'Is the Refugee Convention''s authority rooted in humanitarian imperatives (expansive reading''s axiom) or in the principle that states retain maximum discretion consistent with a minimum floor (restrictive reading''s axiom)?',
    'Examine the Convention''s negotiation history, preamble language, and the Vienna Convention on the Law of Treaties (rules of interpretation). Which framing aligns with the drafting intent and the text''s grammar?',
    'The resolution determines whether this reading is a legitimate interpretation or a misreading. If the Convention''s foundational commitment is humanitarian, the restrictive reading uses canonical text as a cover for sovereignty extraction. If the Convention''s foundational commitment is to preserve state discretion, the expansive reading misreads it as an absolute mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The kernel contest: whether the Convention grounds its authority in humanitarian imperatives or in sovereignty preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(refu_tr_t5, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(refu_tr_t10, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(refu_tr_t15, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(refu_tr_t20, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(refu_tr_t25, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(refu_be_t5, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(refu_be_t10, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(refu_be_t15, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(refu_be_t20, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(refu_be_t25, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(refu_su_t5, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(refu_su_t10, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(refu_su_t15, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(refu_su_t20, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(refu_su_t25, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__restrictive_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% The refugee_convention_text kernel has three instantiations as separate constraint stories, each representing a distinct reading of the same legal text. The restrictive_sovereignty_reading interprets the Convention as a minimum floor permitting maximum state discretion; the expansive_humanitarian_reading interprets it as an unbendable humanitarian mandate; the procedural_integrity_reading prioritizes fair process over outcome. These are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different types. They are linked via network.affects_constraints because they compete for interpretive authority over the same text, and judicial/administrative adoption of one reading constrains the legitimacy space for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
