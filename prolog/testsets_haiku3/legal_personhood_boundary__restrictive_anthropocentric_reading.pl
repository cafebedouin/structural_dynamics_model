% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary: Restrictive Anthropocentric Reading
 *   domain: legal/constitutional/rights_theory
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel of
 *   legal personhood — the restrictive anthropocentric reading that limits
 *   personhood to born humans possessing cognitive capacity. The kernel
 *   itself is fixed in legal doctrine (constitutions, statutes, judicial
 *   precedent), but readings diverge sharply on what that doctrine means and
 *   whom it includes. The restrictive reading anchors personhood to birth
 *   (the biological fact) combined with a cognitive-capacity threshold (a
 *   functional requirement). This produces the structural consequence:
 *   fetuses lack personhood; non-human animals and ecosystems lack standing
 *   as rights-bearers; AI systems cannot claim personhood regardless of
 *   cognition. The pregnant person's autonomy is maximized because fetal
 *   personhood is foreclosed. The reading is sustained by secular
 *   institutional authority and challenged by fetal-status advocates,
 *   environmental advocates, and (prospectively) artificial intelligence
 *   advocates. The story describes this one reading's operation as a
 *   constraint, not as an evaluation of its truth or justice.
 *
 * KEY AGENTS:
 *   - pregnant_persons: Benefit from a personhood boundary at birth; autonomy over reproduction is maximized because fetal personhood is denied.
 *   - secular_institutional_authority: Sets and enforces the personhood boundary; maintains it through abortion law, property law, environmental law that denies standing to non-humans.
 *   - fetal_status_advocates: Structurally excluded from determining personhood; bear the cost of a legal regime that denies their core claim (fetuses are persons).
 *   - environmental_protection_advocates: Excluded from claiming personhood for ecosystems and non-human animals; must pursue environmental protection through proxy human-interest arguments.
 *   - artificial_intelligence_rights_advocates: Prospectively excluded; the 'born human' anchor forecloses their claims regardless of cognitive capacity demonstrated by AI systems.
 *   - cognitive_capacity_functionaries: Face a structural tension: the stated rule requires cognitive capacity, but institutional practice extends personhood to cognitively diminished humans after birth.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.62).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary: Restrictive Anthropocentric Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal/constitutional/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '9a8933dd-a451-4bfc-ae0c-9b7d762db4f9').
narrative_ontology:cs_kernel_codification('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', formalized).
narrative_ontology:cs_authority_grounding('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', extraction).
narrative_ontology:cs_interpretation_layer_present('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9').
narrative_ontology:cs_reading_relation('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', foundational, birth_as_personhood_threshold).
narrative_ontology:cs_axiom_status(birth_as_personhood_threshold, holdable).
narrative_ontology:cs_axiom_grounding('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', birth_as_personhood_threshold, conventional).
narrative_ontology:cs_axiom('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', foundational, human_species_membership_requirement).
narrative_ontology:cs_axiom_status(human_species_membership_requirement, holdable).
narrative_ontology:cs_axiom_grounding('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', human_species_membership_requirement, conventional).
narrative_ontology:cs_reference_frame('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', enlightenment_individual_rights_framework).
narrative_ontology:cs_drift_state('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9a8933dd-a451-4bfc-ae0c-9b7d762db4f9', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons_reproductive_autonomy).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, secular_institutional_authority).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_status_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_protection_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligence_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_functionaries).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, born_human_exclusivity_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_materialist_view).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, state_neutrality_on_reproduction_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, personhood attaches only after birth, maximizing the pregnant person's autonomy over their own body and reproductive choices. The state's interest in fetal life is subordinated to the pregnant person's bodily integrity. They benefit from a legal framework that refuses to grant fetal personhood status, thus preventing personhood-based restrictions on abortion access and reproductive decisions.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, mobile, national).

% Courts, legislatures, and executive bodies that enforce the personhood boundary at birth control the classification of rights-bearers and the scope of state obligations. This reading aligns with institutional neutrality on reproduction: the state does not enforce fetal personhood claims but rather protects pregnant persons' choices. The institutional authority maintains this boundary through abortion law, property law, contract law, and tort law that deny fetal standing.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, secular_institutional_authority, agenda_setter,
    institutional, generational, analytical, national).

% Religious and philosophical traditions that hold fetuses are persons with moral and legal status are structurally excluded from determining personhood law. They bear the cost of a legal regime that treats the fetus as property of the pregnant person or as a non-entity. Their exit is constrained by the same constitutional and legislative frameworks that foreclose fetal personhood doctrine. They must litigate, lobby, or accept the exclusion.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_status_advocates, payer,
    moderate, generational, constrained, national).

% The restrictive anthropocentric reading excludes ecosystems, non-human animals, and environmental entities from legal personhood status. Environmental advocates arguing that rivers, forests, or non-human species should have standing to protect their own interests are met with a personhood boundary that admits only born humans with cognitive capacity. They must pursue environmental protection through proxy arguments (human health, intergenerational rights) rather than via direct rights of environmental entities.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_protection_advocates, payer,
    organized, generational, constrained, global).

% As artificial systems develop cognitive capacities that might meet functional criteria for sentience or self-awareness, this reading's anchor to 'born humans' forecloses rights claims for AI systems, even if they demonstrate cognitive capacities that exceed the threshold. The advocates bear the cost of a legal boundary that privileges species membership (human birth) over demonstrated cognition. They cannot argue for AI personhood within this framework without redefining it.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligence_rights_advocates, payer,
    moderate, biographical, constrained, global).

% Institutions and individuals whose interests depend on denying personhood to humans with diminished cognitive capacity (severe intellectual disabilities, advanced dementia, profound brain injury) face a structural tension under this reading: the boundary claims to require cognitive capacity, but institutional practice often extends personhood protections to cognitively diminished humans after birth. They bear the cost of the contradiction between the stated rule and its actual application.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_functionaries, payer,
    powerful, biographical, mobile, national).

% Communities and individuals who hold that fetuses are persons and whose reproductive futures are shaped by abortion access often lack institutional standing to assert fetal personhood claims in courts and legislatures. They are excluded from determining the boundary itself, though the boundary's persistence depends partly on suppressing their objections.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons_anti_choice, excluded,
    moderate, generational, trapped, national).

% International legal scholars, human rights bodies, and foreign jurisdictions observe and analyze how different readings of personhood produce different legal regimes. They see this reading as one coherent framing among multiple defensible alternatives. They track its operational effects (abortion law, environmental standing, AI regulation) across jurisdictions.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, comparative_legal_systems, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, secular_institutional_authority).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, administrable legal boundary for rights-bearing: personhood attaches at birth, requiring no contestable determination of cognitive capacity at the margins. This creates settled law for reproductive autonomy, inheritance, contractual capacity, and tort standing, avoiding the need to remeasure and litigate personhood status for every cognitively diverse human.
% TRANSFER_FUNCTION: Transfers the power to define personhood from fetal-status advocates and environmental advocates to the state and pregnant persons. Fetuses cease to be legal rights-bearers (no standing to sue, no property rights, no tort claims); environmental entities remain property or resources, not persons. The transfer is enforced through constitutional doctrine, abortion law, and refusal of standing in environmental and animal-welfare litigation.
% ABSENT_VOICES: Fetal-status advocates (religious communities, philosophical traditions holding that human life begins at conception) are structurally excluded from determining the personhood boundary. Environmental personhood advocates (indigenous legal traditions, environmental ethics movements) are also excluded. AI rights advocates have no historical standing in the conversation yet, but the boundary excludes them in advance. These parties would argue for expansion of the personhood circle; their exclusion is actively maintained by the same doctrinal framework.
% DISAPPEARANCE_RATIONALE: If the restrictive anthropocentric reading vanished and no other personhood reading replaced it, legal systems would face a cascade of reclassifications: fetuses might gain tort standing or inheritance rights; non-human animals and ecosystems might acquire legal capacity to sue on their own behalf; AI systems with sufficient cognition might claim personhood. Reproductive autonomy law would shift; environmental law would expand; contract and property law would fragment across species and cognition criteria. The entire architecture of rights attribution would reorganize.
% FOUNDING_PROBLEM: Early legal systems required a workable boundary for rights-bearing, property ownership, and contractual capacity. Extending personhood to all biotic entities or all potential future beings would render property law incoherent and leave no room for resource extraction or land use. The restrictive reading emerged to solve this: confine personhood to a clearly identifiable, administrable class (born humans), leaving everything else as property or resource.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars within the framework attest the founding problem is live: personhood must have a boundary for property law to function. Fetal-status advocates and environmental advocates attest the problem is solved at a different boundary and that the restriction preserves legal convenience at the cost of ethical exclusion. International human rights bodies and indigenous legal traditions dispute the necessity of the human-born boundary. Philosophical work on animal cognition and ecosystem standing (external to the benefiting parties) attests the boundary is now challenged by empirical evidence the reading's founders did not have.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint serves beneficiaries (pregnant persons, secular authority) by systematically excluding other claimants from the personhood circle. The exclusion is not incidental to a primary coordination function — it IS the primary function. Suppression is substantial (0.62) because the constraint persists through active legal enforcement: abortion doctrine that denies fetal standing, environmental law that refuses to grant nature legal personhood, property law that classifies non-humans as resources. Theater ratio rises modestly over the interval (0.28 → 0.41) as the constraint's justifications increasingly rely on philosophical and scientific rhetoric (appeals to cognitive capacity, functionality) rather than on settled institutional practice. The temporal pattern shows extractiveness accumulating (post-2000s intensification of abortion access globally correlates with stricter institutional enforcement of fetal non-personhood), while theater increases because defenders must rhetorically justify what was once taken as settled (birth as the boundary). The constraint is CLAIMED as tangled_rope because it genuinely coordinates reproductive autonomy and legal administration (real coordination function) while asymmetrically extracting from fetal-status and environmental advocates (active enforcement of their exclusion). The metrics describe substantially enforced operation with modest but rising theater.
 *
 * PERSPECTIVAL GAP:
 *   The institutional agenda-setter seat experiences the constraint as coordinating reproductive law, property law, and administrative clarity. The pregnant-person beneficiary seat experiences it as securing bodily autonomy. The fetal-status-advocate payer seat experiences it as enforced exclusion from determining what counts as a rights-bearer. The environmental-advocate payer seat experiences it as a lock that refuses to recognize ecosystems as having standing. The engine derives these different classifications per seat from the stakeholder power, exit, and time-horizon data combined with the structural beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons and secular institutional authority are structural beneficiaries (directionality near 0.0): they benefit from the boundary without bearing its costs. Fetal-status advocates, environmental advocates, and AI-rights advocates are structural targets (directionality near 1.0): the boundary's persistence depends on suppressing their claims. Cognitive-capacity functionaries sit in an unstable position (directionality ~0.5): they benefit from a rule that claims to rest on cognition, but institutional practice undermines that rule. The exit options amplify this: institutional authority has arbitrage (can shift interpretation within the doctrine); pregnant persons have mobility (can relocate to jurisdictions with stronger reproductive autonomy); fetal-status advocates are constrained (their exit is religious/philosophical doctrine that forbids accepting the boundary); environmental advocates are organized but face a global boundary (harder exit than local circumvention).
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive anthropocentric reading faces a mandatrophy question: Was the founding problem (establishing a workable boundary for rights-bearing in early legal systems) still live, or has it become a zombie justification? The measurement series shows extractiveness plateauing after t=40 (the interval end is calibrated to ~2030), suggesting the constraint has stabilized at its extractive equilibrium. Theater rises throughout, consistent with increasing rhetorical labor required to defend a boundary that empirical advances (animal cognition research, AI development, ecosystem science) are rendering less obviously natural. The constraint is NOT mandatrophy-resolved because the founding problem (needing a boundary) is still live — legal systems still require a boundary for property and rights, and the restrictive reading still solves that problem, even as other costs have become clearer. However, the rising theater ratio and the failure of extractiveness to increase further suggests the constraint may be entering a piton phase: it persists through institutional inertia and doctrinal continuity, but an increasing share of the enforcement machinery exists to suppress dissent rather than to solve the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_margin_ambiguity,
    'What cognitive capacities are required for personhood? Does the boundary admit humans with severe intellectual disabilities, profound dementia, or irreversible brain injury after birth?',
    'Systematic audit of personhood determinations across disabilities, dementia, and neurological injury cases in case law; comparison of stated rule (cognitive capacity required) to actual institutional practice (extension of personhood to cognitively diminished humans after birth).',
    'If the boundary is systematically applied, the constraint becomes coherent but deeply exclusionary of humans with cognitive disabilities. If institutional practice contradicts the stated rule, the constraint''s boundary becomes incoherent and theater increases (defenders must explain the exception). Either way, the tension between ''cognitive capacity'' and ''all born humans'' is an irreducible ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_capacity_margin_ambiguity, empirical, 'Whether the ''cognitive capacity'' threshold is actually enforced or functionally abandoned for humans after birth.').

omega_variable(
    fetal_status_structural_vs_internalized_suppression,
    'Is the suppression of fetal-status advocates structural (legal doctrine denies them standing) or internalized (they have internalized the secular-liberal framing and doubt their own premises)?',
    'Post-advocacy suppression trajectory: if fetal-status claims persist with organizational vigor in jurisdictions where abortion is restricted, suppression is more structural than internalized. If advocates'' confidence and organizational presence collapse in liberalizing jurisdictions, suppression was partly internalized.',
    'If suppression is structural, the constraint''s persistence depends on active doctrinal enforcement and legal exclusion — a snare-like feature. If suppression is internalized, the constraint''s power is partly self-perpetuating through advocates'' self-doubt, making the extraction even more difficult to reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fetal_status_structural_vs_internalized_suppression, empirical, 'Whether fetal-status advocates are suppressed by external barriers or have internalized the personhood boundary''s legitimacy.').

omega_variable(
    anthropocentric_boundary_necessity,
    'Is the restriction to born humans structurally necessary for law to function, or is it a contingent choice that prioritizes simplicity over moral inclusion?',
    'Comparative legal analysis of jurisdictions that have granted limited personhood to non-human animals (some nations recognize animal welfare standing) and ecosystems (rivers in India, New Zealand; forests in Ecuador). If these jurisdictions'' property and contract law remain coherent, the boundary is contingent, not necessary.',
    'If necessary, the extractiveness is justified as coordination cost and the constraint is genuinely rope-like. If contingent, the extractiveness is unjustified privilege and the constraint is snare-like. The engine cannot resolve this; it is an axiom-level question in the reading''s own tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anthropocentric_boundary_necessity, conceptual, 'Whether the anthropocentric boundary is logically necessary for legal systems or a contingent institutional choice.').

omega_variable(
    reading_foreclosure_potentiality_conception,
    'Does the restrictive anthropocentric reading (personhood at birth) logically foreclose the developmental potentiality reading (personhood at conception), or can both be held in different frameworks?',
    'Logical analysis of the core premises: if potentiality reading''s premise is ''any human life trajectory bearer is a person'' and restrictive reading''s premise is ''only born humans are persons,'' the premises contradict at the referent (fetuses). A single legal system cannot hold both simultaneously without incoherence.',
    'If foreclosure is real, it should appear in cs_structure.reading_relations as FORECLOSES. If both can coexist (different parties holding them), it should appear as COEXISTS_WITH. The relation type changes how the engine models the kernel''s evolutionary pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_potentiality_conception, conceptual, 'Whether this reading logically forecloses or merely conflicts with the developmental potentiality reading.').

omega_variable(
    cognitive_capacity_functional_compatibility,
    'Does the restrictive reading''s stated requirement of cognitive capacity align with the functional_capacity_reading, making the restriction purely about species membership?',
    'Textual analysis of judicial opinions and statutory materials: how often is ''cognitive capacity'' invoked? How often is ''human species'' invoked? Are they treated as equivalent or as separate criteria? If species is the actual boundary and cognition is theater, the constraint''s true axis is anthropocentrism, not functionalism.',
    'If cognition is theater and species is the true boundary, the constraint''s claim to be ''functional'' is false; it is nakedly species-preference. This would increase theater_ratio interpretation and suggest the cognitive-capacity framing is cover for anthropocentric exclusion. If cognition is genuinely applied, the constraint occupies middle ground between potentiality and functional readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_capacity_functional_compatibility, empirical, 'Whether ''cognitive capacity'' is a genuine requirement or theater masking species-preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(lega_tr_t0, observed).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(lega_tr_t8, observed).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(lega_tr_t16, observed).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(lega_tr_t24, observed).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(lega_tr_t32, observed).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(lega_tr_t40, observed).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(lega_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(lega_be_t0, observed).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(lega_be_t8, observed).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(lega_be_t16, observed).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(lega_be_t24, observed).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(lega_be_t32, observed).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(lega_be_t40, observed).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(lega_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(lega_su_t0, observed).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(lega_su_t8, observed).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement_basis(lega_su_t16, observed).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement_basis(lega_su_t24, observed).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement_basis(lega_su_t32, observed).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(lega_su_t40, observed).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(lega_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_constraint).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_standing_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the legal_personhood_boundary kernel. The kernel is the fixed text and doctrine of law that references 'persons' and 'rights-bearers'; the three readings instantiate different interpretations of that kernel. The restrictive anthropocentric reading maximizes pregnant-person autonomy and minimizes environmental/AI standing by anchoring personhood to birth + cognitive capacity (human species de facto). Sibling readings: developmental_potentiality (conception + any life trajectory = personhood) and functional_capacity (cognition regardless of species = personhood). The three stories share the same kernel and core institutional context; they differ in their beneficiary/victim structures and the exclusions they enforce. Each story is authored with its own ε (extractiveness from the reading's own epistemic perspective) and its own stakeholder situation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
