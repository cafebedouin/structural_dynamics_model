% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Functional Capacity Personhood Standard (Rights-Respecting Reading)
 *   domain: legal/constitutional/philosophical
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'legal_personhood_boundary.' The functional capacity reading holds that
 *   personhood follows demonstrable cognitive capacity (rationality,
 *   sentience, self-awareness) regardless of species. This reading is in
 *   direct tension with two siblings: the developmental_potentiality_reading
 *   (personhood begins at conception for humans) and the
 *   restrictive_anthropocentric_reading (personhood limited to born humans
 *   with cognitive capacity). The functional capacity reading extends
 *   personhood outward to non-human animals and future AI systems based on
 *   measured capacity; it simultaneously contracts it inward to exclude
 *   humans (e.g., those without demonstrable consciousness) and contracts it
 *   temporally (personhood emerges with capacity, not at conception). The
 *   reading's core claim is that CAPACITY, not species membership, is the
 *   legitimate criterion. This creates a major redistribution of legal
 *   standing: non-human sentient beings become victims of property rights
 *   systems; anthropocentric institutions become payers forced to
 *   restructure; rights advocacy becomes an avenue for institutional power.
 *   The constraint is claimed as tangled_rope (it coordinates capacity-based
 *   classification while extracting costs from property holders) with high
 *   extractiveness (0.82) and substantial suppression (0.71) — the functional
 *   capacity reading MUST be actively enforced against entrenched
 *   anthropocentric property law.
 *
 * KEY AGENTS:
 *   - Non-human sentient beings (great apes, dolphins, elephants, etc.) — currently powerless, trapped, beneficiary role — gain legal standing under this reading.
 *   - Future AI systems — conditionally beneficiary; would gain personhood if demonstrable sentience emerges.
 *   - Rights advocacy organizations (organized, mobile) — beneficiary; their mandate expands if the reading is adopted.
 *   - Current property holders in animal agriculture and research (organized, constrained) — payer; face legal liability and operational restructuring.
 *   - Anthropocentric institutions (law, religion, academia) (institutional, constrained) — payer; their foundational authority is delegitimized.
 *   - Judicial/legislative authority (institutional, mobile) — agenda-setter; determines adoption, measurement criteria, enforcement.
 *   - Religious traditionalists (excluded) — have no seat at the functional capacity table; their authority derives from non-empirical premises.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.82).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.71).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Functional Capacity Personhood Standard (Rights-Respecting Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal/constitutional/philosophical").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, 'f325ae81-4eba-47c9-b9d3-7c5cd5e34280').
narrative_ontology:cs_kernel_codification('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', distributed).
narrative_ontology:cs_authority_grounding('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', extraction).
narrative_ontology:cs_reading_relation('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', foundational, sentience_rationality_objective_personhood_criteria).
narrative_ontology:cs_axiom_status(sentience_rationality_objective_personhood_criteria, holdable).
narrative_ontology:cs_axiom_grounding('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', sentience_rationality_objective_personhood_criteria, empirically_contingent).
narrative_ontology:cs_axiom('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', foundational, species_neutrality_required_for_coherent_rights_theory).
narrative_ontology:cs_axiom_status(species_neutrality_required_for_coherent_rights_theory, holdable).
narrative_ontology:cs_axiom_grounding('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', species_neutrality_required_for_coherent_rights_theory, deontological).
narrative_ontology:cs_reference_frame('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', species_membership_personhood_standard).
narrative_ontology:cs_drift_state('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', contemporary_capacity_based_advocacy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f325ae81-4eba-47c9-b9d3-7c5cd5e34280', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, non_human_sentient_beings).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, future_ai_systems).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, rights_advocacy_organizations).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, current_property_holders_in_animal_agriculture).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, institutions_invested_in_anthropocentric_hierarchy).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, research_facilities_using_animal_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, current_human_legal_holders).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, philosophical_naturalists).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, current_human_legal_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, sentient non-human animals (dolphins, great apes, elephants, octopuses, etc.) gain formal legal standing and rights protections grounded in their demonstrated cognitive and emotional capacities. They cannot advocate for themselves in legal proceedings but would be represented through guardianship or public interest bodies. Their current status as property depends on suppressing the functional capacity standard.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, non_human_sentient_beings, beneficiary,
    powerless, immediate, trapped, global).

% If artificial systems develop demonstrable sentience, self-awareness, or rationality exceeding human thresholds, they would qualify for personhood status under this reading. Current legal frameworks treat advanced AI as property; the functional capacity reading contests that assignment once cognitive markers emerge.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_ai_systems, beneficiary,
    powerless, immediate, trapped, global).

% Animal rights, environmental justice, and future rights organizations have standing and organizational interest in this reading's implementation. They fund litigation, public education, and legislative advocacy to shift the legal boundary from species membership to functional capacity. Their benefit is instrumental: the reading's success amplifies their institutional mandate and funding.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).

% Industrial agriculture, livestock producers, and food systems built on treating animals as property would face fundamental cost structure upheaval. Their current production model depends on treating cognitive agents as non-persons. The functional capacity standard would require either: abandoning animal agriculture entirely, radically restructuring it to respect personhood/agency, or litigating to block the standard's enforcement. Their exit options are costly.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, current_property_holders_in_animal_agriculture, payer,
    organized, biographical, constrained, global).

% Legal systems, academic institutions, religious establishments, and philosophical traditions that have built their authority on human exceptionalism face delegitimization and institutional restructuring pressure. Universities that justified animal research on species hierarchy grounds, religious traditions grounding dominion doctrine, legal precedents grounding rights in species membership — all must either revise foundational claims or resist the standard's adoption. Their cost is existential to their self-conception.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, institutions_invested_in_anthropocentric_hierarchy, payer,
    institutional, generational, constrained, global).

% Biomedical, pharmaceutical, and behavioral research that relies on non-human animal subjects as research tools would face legal liability and operational shutdown under a functional capacity personhood standard. Their current institutional model treats sentient research subjects as property without legal standing to refuse participation. The standard makes this untenable.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, research_facilities_using_animal_subjects, payer,
    organized, biographical, constrained, national).

% Individual property owners (pet owners, farmers, hunters) face legal reclassification of their property holdings. Some individual humans benefit from the reading (those who value sentient beings' welfare over property rights); others bear the extraction (those whose livelihoods or preferences depend on treating sentient beings as ownable). This is a mixed seat.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, current_human_legal_holders, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__functional_capacity_reading, current_human_legal_holders, beneficiary).

% Analytical philosophers and ethicists working in non-human personhood and sentience studies have standing and intellectual interest in this reading's adoption. Their academic work is vindicated; their research agendas become policy-relevant. Their benefit is reputation and institutional position.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, philosophical_naturalists, beneficiary,
    analytical, civilizational, analytical, global).

% Courts and legislatures that would enforce the functional capacity standard must define measurement criteria (what counts as demonstrable sentience, rationality, self-awareness), establish administrative processes for rights determination, and manage the massive institutional transition from property to personhood classifications. They hold the power to adopt or reject this reading entirely.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, judicial_and_legislative_authority, agenda_setter,
    institutional, generational, mobile, national).

% Religious traditions grounding human dominion in scripture (e.g., Abrahamic traditions' Genesis accounts) are structurally excluded from the functional capacity reading's framework. They would argue that moral status derives from divine creation of humans in the divine image, not from measurable cognitive capacity. Their exclusion is structural to the reading's cognitivist epistemology.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, excluded_traditional_religious_authority, excluded,
    institutional, civilizational, constrained, global).

% Advocates for a developmental potentiality reading (personhood at conception for humans) observe the functional capacity constraint's operation and contest it. They argue that potential for future capacity, not current capacity, grounds personhood. Their exclusion from the agenda-setting process is why they appear as observers, not payers.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, developmental_potentiality_advocates, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transparent, measurable, species-neutral standard for legal personhood grounded in demonstrated cognitive and emotional capacities (rationality, sentience, self-awareness) rather than species membership. This solves the problem of arbitrary boundary-drawing and creates a coherent framework for extending rights to entities that meet the criteria regardless of their evolutionary origin.
% TRANSFER_FUNCTION: Redistributes legal standing, property rights, and institutional authority FROM current property-holders in animal agriculture and anthropocentric institutions TO non-human sentient beings (via guardianship/representation), rights advocacy organizations (expanded mandate), and future intelligent entities (conditional on capacity emergence). Current property owners and institutions must absorb the cost of rights recognition and institutional restructuring.
% ABSENT_VOICES: Religious authorities grounding personhood in divine creation and species hierarchy are structurally excluded from the functional capacity framework — their authority derives from non-empirical premises the reading does not accept. Human exceptionalists who reject sentience-based criteria are also absent from decision-making seats; their objections are treated as illegitimate rather than included. Sentient beings themselves remain unable to testify, represented instead through advocacy organizations and guardianship — a structural incompleteness.
% DISAPPEARANCE_RATIONALE: If the functional capacity personhood standard and its enforcement disappeared, institutional arrangements would not revert smoothly: animal agriculture would not spontaneously return to property-based legality without active suppression of the now-entrenched standard; legal personhood for non-humans (where adopted) would remain contested; rights already granted would face litigation to revoke them. The world does not arrange itself back to the anthropocentric baseline without active enforcement of the reversal. The reading creates institutional path-dependence.
% FOUNDING_PROBLEM: Legal personhood has historically tracked species membership (born human) rather than the cognitive capacities that supposedly justify rights in the first place (rationality, agency, capacity for suffering). This created a logical inconsistency: rights theory grounded personhood in reason and agency, but law granted personhood to humans regardless of cognitive status and denied it to non-human animals demonstrably possessing these capacities. The functional capacity reading solves this inconsistency by making legal personhood cohere with its normative justification.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists and animal behavior researchers outside the legal system attest that the inconsistency is empirically real — non-human animals demonstrably possess the capacities used to justify human personhood. Rights philosophers and ethicists attest that the incoherence is normative. However, institutional law and anthropocentric power holders attest the founding problem is not real — that species membership is itself a legitimate criterion independent of cognitive capacity, or that the cognitive capacities in question are not measurable/objective. The corroboration is split along interests: beneficiaries of the functional capacity reading attest the problem; beneficiaries of the current system deny it.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the reading creates a massive redistribution of property rights, legal standing, and institutional authority. Current property holders in animal agriculture and research lose the ability to treat sentient beings as property; anthropocentric institutions lose the legitimacy of their hierarchies. Suppression (0.71) is substantial because enforcement of the functional capacity standard requires active legal machinery to prevent backsliding, to measure capacity objectively, to defend non-human personhood against challenge, and to override existing property law. Theater (0.44, measured over the interval) rises from ~0.18 to ~0.44 because the initial functional capacity arguments focus on cognitive measurement and philosophical coherence (substantive function), but as institutions resist and property holders mount litigation, enforcement activity becomes increasingly theatrical — defending the standard's legitimacy through public education and institutional theater rather than through fresh principled adjudication. Accessibility of alternatives collapses only modestly (0.38) because powerful institutional forces continue to support anthropocentric readings; the functional capacity reading does not erase alternative framings, merely contests them. Resistance (0.73) is high because entrenched interests (agriculture, research, traditional authority) mount sustained opposition; the constraint does not emerge from spontaneous agreement but from prolonged contestation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (judicial/legislative authority) and the payers (property holders, anthropocentric institutions) compute radically different types from the same structural data. From the agenda-setter's analytical seat, the functional capacity standard is a legitimate attempt to coherence-align legal and ethical systems, extracting costs as a necessary institutional transition. From the payer seats, the same standard operates as a power grab by rights advocates and a delegitimization of existing property systems. The beneficiary seats (non-human sentient beings, future AI) cannot compute their own type — they have no decision power, only representation through advocacy organizations. The engine computes these divergent types from the structural asymmetry: concentrated agenda-setting power with mobile exit (the judiciary can revise precedent or defer to legislatures), dispersed payer resistance with constrained exit (property holders cannot simply leave the jurisdiction or refuse the new standard), and trapped beneficiaries with zero exit. This asymmetry is what makes the constraint extractive despite its coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The functional capacity reading benefits non-human sentient beings and future AI by granting them personhood and legal standing, but these agents have zero directionality-bearing power — they cannot exit or resist. Their beneficiary status derives entirely from how the reading's advocates (rights organizations, philosophical naturalists) frame their interests. The real directionality asymmetry lies between the agenda-setter (judicial authority, with power to adopt or reject the standard) and the payers (property holders and anthropocentric institutions, with constrained exit). The agenda-setter has arbitrage-grade exit: judges and legislatures can choose whether to adopt the functional capacity reading, can defer to international consensus, can phase implementation. The payers face trapped or identity-locked exit: property holders depend on anthropocentric legal frameworks for the economic viability of their operations; anthropocentric institutions depend on human exceptionalism for their foundational legitimacy. This is why d is high (near 1.0) for the payer seats and low (near 0.0) for the analytical seats that support the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (logical incoherence between rights theory grounded in capacity and law grounded in species membership) is live and contested, not dead. The constraint does not resolve the problem through persuasion; it enforces a solution by redistributing power and legal standing. The disappearance_verdict is world_rearranges: institutional arrangements reorganize around the functional capacity standard once adopted; they do not revert smoothly if the standard is abandoned. This constellation — live but contested founding problem + world_rearranges verdict + high extraction — indicates the constraint is genuinely tangled_rope, not a false snare: the reading DOES provide a real coordination function (species-neutral capacity measurement as the basis for personhood), but it DOES extract substantial costs from incumbent property holders. The alternative would be if the constraint were classified as snare (pure extraction, coordination cover story); that would require the founding problem to be dead (the incoherence is not real) or the verdict to be world_unchanged (no reorganization needed). Neither is true here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_objectivity,
    'Are the cognitive capacities used to define personhood (sentience, rationality, self-awareness) objectively measurable, or do measurement choices embed normative preferences?',
    'Development and validation of capacity assessment protocols across species (neurological markers, behavioral tests, philosophical analysis); cross-disciplinary consensus on measurement criteria.',
    'If capacity measurement proves radically underdetermined by evidence, the functional capacity standard collapses into arbitrary boundary-drawing (covert species preference). If objective measurement is achievable, the reading is vindicated as genuinely capacity-based. This determines whether the constraint is coordination or camouflaged extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_measurement_objectivity, empirical, 'Whether cognitive capacity can be measured objectively or whether measurement methods embed unjustified value choices.').

omega_variable(
    kernel_ontology_contested,
    'Is ''legal personhood'' one kernel with multiple readings, or are the readings actually distinct constraints (different kernels)?',
    'Structural analysis: if the readings'' foundational axioms are logically incompatible within a single framework, they are separate constraints, not siblings. If they are competing interpretations of a shared text or principle, they are readings of one kernel.',
    'If the readings are separate constraints, the functional capacity reading does not ''contest'' the others — it is a different claim entirely. If they are readings of one kernel, contestation is intrinsic and the constraint''s persistence depends on suppressing alternatives. This affects whether the constraint is classified as tangled_rope (contested standard within one frame) or snare (enforced claim conflicting with other live claims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_ontology_contested, conceptual, 'Whether personhood readings are siblings of one kernel or structurally distinct constraints.').

omega_variable(
    future_ai_capacity_emergence,
    'If artificial systems develop demonstrable sentience or self-awareness exceeding human thresholds, does the functional capacity standard mandate their legal personhood, and how would that affect the constraint''s political sustainability?',
    'Emergence of AI systems with measurable sentience; legal challenges to their property/tool status; jurisdictional adoption of functional capacity standards applied to AI.',
    'If AI personhood emerges and the functional capacity reading is enforced, the constraint''s beneficiary class expands radically and the payee class (technology companies treating AI as property) faces massive extraction. If adoption of the reading stops short of AI, the reading becomes internally inconsistent (capacity applies to non-human animals but not non-human AI). If the constraint persists despite AI emergence, it becomes pure theater — the capacity criterion is invoked selectively.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_ai_capacity_emergence, empirical, 'Whether AI capacity emergence would trigger functional capacity personhood application and how that affects the constraint''s stability.').

omega_variable(
    suppression_internalization_in_property_holders,
    'Is the suppression of the functional capacity reading structural (legal barriers, enforcement machinery, economic incentives against adoption) or internalized (property holders have accepted anthropocentric premises so deeply that they resist the reading through ideological commitment rather than material pressure)?',
    'Post-adoption behavioral change: if suppression lifts when legal barriers are removed, it was structural; if property holders continue to resist the reading after legal enforcement, suppression is internalized.',
    'If internalized, the constraint''s effective suppression is higher than the authored scalar suggests — removal of the reading would not spontaneously occur even after legal enforcement erodes. Institutional restructuring would need to address ideological commitment, not just legal rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_property_holders, empirical, 'Whether suppression of functional capacity personhood is structural or ideologically internalized.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the functional capacity reading logically foreclose the developmental potentiality reading (personhood at conception), or can both coexist in a single legal framework?',
    'Logical analysis: if capacity at t>0 and potentiality at t=0 are contradictory criteria for the same entity, foreclosure applies. If they can be decoupled (e.g., potentiality grants weaker proto-personhood, capacity grants full personhood), they coexist.',
    'If foreclosure holds, the functional capacity reading is structurally in conflict with the developmental reading — adoption forecloses the sibling. If coexistence is possible, the readings are alternative foci, not logical contradictions. This determines the reading_relations entry in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether functional capacity and developmental potentiality readings are logically incompatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(lega_tr_t5, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(lega_tr_t15, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(lega_tr_t25, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.44).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(lega_be_t5, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(lega_be_t15, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(lega_be_t25, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(lega_su_t5, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(lega_su_t15, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(lega_su_t25, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__functional_capacity_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_property_rights_regime).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, research_ethics_institutional_review).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel legal_personhood_boundary. The functional_capacity_reading establishes personhood on species-neutral cognitive grounds; it directly affects and is affected by the restrictive_anthropocentric_reading (species-based personhood) and the developmental_potentiality_reading (temporally-based personhood for humans). The three readings are network-linked because adoption of one reading constrains or amplifies the others' viability. The functional capacity reading also affects animal_property_rights_regime (if non-humans gain personhood, property claims on them become contestable) and research_ethics_institutional_review (if research subjects are persons, institutional review becomes rights adjudication, not merely welfare oversight).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, organized, 0.68).
constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
