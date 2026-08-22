% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__property_reading, []).

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
 *   constraint_id: animal_status_kernel__property_reading
 *   human_readable: Animals as Property: Economic Value Reading
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested animal-status
 *   kernel: the property reading, which asserts that animals are property,
 *   moral considerability derives from ownership rights, and economic value
 *   is the only relevant evaluative metric. This is one of three incompatible
 *   readings of what 'animal status' means; the other two (welfare and
 *   abolitionist) are authored as separate constraint stories in the
 *   animal_status_kernel family and linked through network relationships. The
 *   property reading is claimed as rope (legitimate coordination enabling
 *   predictable commercial use) while authored metrics describe substantially
 *   extractive operation with rising theater ratio—the divergence is
 *   intentional and diagnostic. The constraint's persistence depends not on
 *   spontaneous participant agreement but on active enforcement of the
 *   property classification and suppression of countervailing moral
 *   frameworks.
 *
 * KEY AGENTS:
 *   - animal_owners: institutional actors (farmers, researchers, entertainment operators) who control animal bodies and depend on property status for decision authority
 *   - extractive_industries: pharmaceutical, agricultural, and resource-extraction sectors whose business models presuppose unrestricted animal use
 *   - legal_and_regulatory_authorities: codify the property classification and enforce it via narrow anti-cruelty statutes interpreted as protecting property value
 *   - moral philosophers and welfare advocates: excluded from the conversation by the framework's stipulation that economic value is the only relevant metric
 *   - consumers: benefit from low-cost products while bearing diffuse costs the framework excludes from consideration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.91).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.72).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, rope).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animals as Property: Economic Value Reading").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '308e75e7-1b57-4ee3-9f61-3573adbdc361').
narrative_ontology:cs_kernel_codification('308e75e7-1b57-4ee3-9f61-3573adbdc361', formalized).
narrative_ontology:cs_authority_grounding('308e75e7-1b57-4ee3-9f61-3573adbdc361', extraction).
narrative_ontology:cs_interpretation_layer_present('308e75e7-1b57-4ee3-9f61-3573adbdc361').
narrative_ontology:cs_reading_relation('308e75e7-1b57-4ee3-9f61-3573adbdc361', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('308e75e7-1b57-4ee3-9f61-3573adbdc361', animal_status_kernel__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('308e75e7-1b57-4ee3-9f61-3573adbdc361', foundational, animals_are_property).
narrative_ontology:cs_axiom_status(animals_are_property, holdable).
narrative_ontology:cs_axiom_grounding('308e75e7-1b57-4ee3-9f61-3573adbdc361', animals_are_property, conventional).
narrative_ontology:cs_axiom('308e75e7-1b57-4ee3-9f61-3573adbdc361', foundational, economic_value_supreme_metric).
narrative_ontology:cs_axiom_status(economic_value_supreme_metric, holdable).
narrative_ontology:cs_axiom_grounding('308e75e7-1b57-4ee3-9f61-3573adbdc361', economic_value_supreme_metric, instrumental).
narrative_ontology:cs_axiom('308e75e7-1b57-4ee3-9f61-3573adbdc361', secondary, owner_discretion_principle).
narrative_ontology:cs_axiom_status(owner_discretion_principle, holdable).
narrative_ontology:cs_axiom_grounding('308e75e7-1b57-4ee3-9f61-3573adbdc361', owner_discretion_principle, conventional).
narrative_ontology:cs_reference_frame('308e75e7-1b57-4ee3-9f61-3573adbdc361', property_rights_supremacy_framework).
narrative_ontology:cs_drift_state('308e75e7-1b57-4ee3-9f61-3573adbdc361', contemporary_resistance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('308e75e7-1b57-4ee3-9f61-3573adbdc361', '2026-08-03T14:32:00Z').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, extractive_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, consumers).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess legal title to animals; determine use, disposal, and treatment subject only to narrow anti-cruelty statutes that protect property value. They set the agenda by defining what counts as justified use and what uses are economically rational. Their interest is unrestricted decision authority and alienability of the resource.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_owners, agenda_setter,
    powerful, generational, arbitrage, global).

% Agriculture, pharmaceutical testing, entertainment, and resource extraction depend on unrestricted use of animals as inputs. The property framework legitimizes high-volume extraction without moral constraint. They benefit from the framework's immunity to pain-based objections.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, extractive_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Are subject to owner discretion without countervailing rights. Under this reading, animals have no standing to object because they are classified as property, not moral patients. They bear the costs of unrestricted use but have no recognized interests in the arrangement. Listed as non-agent entity per the OQ-64 protocol: animals in this reading are excluded from the agent-hood that would enable beneficiary/victim derivation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animals, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__property_reading, animals).

% Codify and enforce the property classification. They administer anti-cruelty statutes, which are interpreted narrowly to protect owner investment rather than animal welfare. They both set the legal framework and occupy an observer seat insofar as they could, in principle, reclassify animals morally.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legal_and_regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, legal_and_regulatory_authorities, observer).

% Would argue that animal sentience or intrinsic rights demand moral status independent of ownership. They are excluded from the conversation by the framework's stipulation that economic value is the sole relevant value; their objections are treated as external to the constraint's logic, not as live alternatives within it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, moral_philosophers_and_advocates, excluded,
    moderate, generational, constrained, global).

% Benefit from low-cost animal products made possible by treating animals as unrestricted resources. They also bear diffuse costs insofar as the framework enables extreme confinement, suffering, and environmental externalities that are not priced into goods. Their awareness of these costs is suppressed by the framework's prohibition on considering animal interests in purchasing decisions.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, consumers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, consumers, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__property_reading, animal_owners).
narrative_ontology:fixing_cost_class(animal_status_kernel__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified rule that animals are property subject to owner discretion, which enables predictable, unambiguous commercial use of animals across jurisdictions and eliminates friction from welfare-based objections to routine practices.
% TRANSFER_FUNCTION: Transfers decision authority over animal bodies and lives from any potential countervailing moral consideration to owners and extractive industries. Economic value—the capacity to convert animals into commodities—flows to owners and industrial beneficiaries; the animal experiences (suffering, confinement, death) flow in the opposite direction without compensation or moral reckoning.
% ABSENT_VOICES: Animals themselves cannot participate in the conversation; philosophers and advocates arguing for sentience-based or rights-based moral status are excluded by the framework's stipulation that only economic value counts. Alternative readings (welfare, abolitionist) are present in some jurisdictions but kept out of this particular institutional framework by definitional fiat.
% DISAPPEARANCE_RATIONALE: If the property status and unrestricted-use framework disappeared overnight, commercial animal agriculture would contract severely, pharmaceutical and research protocols would require alternative models, and the trillions of animals currently used annually in production systems would either no longer exist in those systems or would exist under fundamentally different use constraints. The economic geography of food, medicine, and industry would reorganize around different input assumptions.
% FOUNDING_PROBLEM: Animals are economically valuable resources; property law needed a framework to enable predictable ownership, sale, and use without constant re-negotiation of use rights. The founding problem is not animal welfare but the need for a stable property regime that maximizes resource extraction.
% FOUNDING_PROBLEM_CORROBORATION: The property-owning and extractive-industry seats attest the founding problem remains live: commercial animal agriculture, research, and entertainment depend on clear ownership and use rights. Welfare and abolitionist advocates dispute this verdict entirely, arguing the 'founding problem' is a rationalization for unjust status; they attest the founding problem is actually the exclusion of animal sentience from moral consideration. No neutral party outside both beneficiary and objecting seats can corroborate the 'liveness' of the founding problem—the verdict itself is internal to the reading.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.91, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__property_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.91) because the constraint eliminates any countervailing moral consideration—animals have no standing to object, no recognized interests, no participatory voice. Suppression is substantial (0.72) because the framework requires active enforcement: philosophical objections must be excluded from legal reasoning, welfare concerns must be reframed as property-protection, and dissenting readings must be kept out of institutional decision-making. Theater ratio is low (0.18) because the property framework operates relatively openly—it does not pretend to maximize animal welfare; it openly subordinates animal interests to owner discretion. The measurement series tracks rising extractiveness over time as commercial intensification of animal use accelerates without moral constraint, and rising theater ratio as regulatory rhetoric shifts toward rhetorical welfare concessions (e.g., 'humane slaughter,' 'animal welfare standards') without fundamentally altering the property-and-extraction structure. Suppression rises as resistance from animal advocates and welfare constituencies grows and must be managed through institutional gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (owners, extractive industries, legal authorities) perceive the constraint as a necessary coordination mechanism that clarifies ownership and enables productive economic activity. The excluded seat (moral philosophers) perceives it as a foundational injustice—the illegitimate exclusion of sentient beings from moral status. The consumer seat occupies a paradoxical position: they benefit materially but are suppressed from recognizing that benefit depends on denying moral status to sentient beings. The abolitionist and welfare readings occupy a fourth and fifth perspective, each of which would computationally diverge from this one. The engine should compute radically different type-classifications for the same constraint evaluated from these different seats—this is the manifestation of the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   From the owner/industry seat, the arrangement is legitimate coordination: a stable property rule that enables efficient resource allocation and predictable commercial use. From the animal perspective (which the reading excludes from moral agency), the same structure is comprehensive extraction—unrestricted use without consent or compensation. From the consumer seat, the arrangement provides low-cost goods but suppresses awareness of the costs borne by animals. From the advocate seat, the arrangement is illegitimate per se because it denies moral status to sentient beings. The engine computes these divergent directionalities from the structural data: owner/industry near d=0.0 (beneficiary), animals near d=1.0 (target, though excluded from agent-hood), consumers near d=0.4 (modest benefits, diffuse costs), advocates excluded entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a stable property regime enabling commercial animal use) is declared 'live' by beneficiary seats but is precisely what welfare and abolitionist readings declare 'dead'—the founding coordination problem has been displaced by the extraction problem. This mismatch (founding_problem_status=live x disappearance_verdict=world_rearranges) is a mandatrophy candidate: if the property reading resolved the genuine coordination problem it claims, alternative readings would have no ground to stand on. But the persistence of welfare and abolitionist challenges, the rising theater ratio (humane slaughter rhetoric that does not alter property status), and the measurement-tracked suppression escalation all suggest the constraint persists not because it solved a coordination problem but because it locks in extraction for beneficiary seats. No seat that is not already aligned with the property reading gains genuine benefit from it; the constraint persists by institutional inertia and active suppression of alternatives, not by spontaneous agreement. This is the signature of a constraint whose mandate has outlived its legitimating function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_moral_relevance_contest,
    'Is animal sentience (the capacity to suffer) morally relevant independent of property status and economic value? Does the property reading''s exclusion of sentience-based objections rest on a defensible normative premise or merely on stipulation?',
    'This is not an empirical question but a foundational axiom dispute. The welfare and abolitionist readings dispute the property reading''s core premise. Resolution would require an external meta-framework to adjudicate which reading is justified—no such framework is available outside the readings themselves. The dispute is recursive and non-resolvable within the Deferential Realism apparatus; the apparatus''s job is to model the structure of the dispute, not to resolve it.',
    'If sentience IS morally relevant, the property reading''s high extractiveness and suppression reflect an unjust status quo, not legitimate coordination. If sentience is NOT morally relevant (the property reading''s stipulation), then the high extractiveness reflects only the cost of denying standing to non-property-bearing entities—which may be coherent but requires defending the foundational axiom against abolitionist and welfare objections.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_moral_relevance_contest, conceptual, 'Kernel contest: whether sentience is morally relevant and whether property status is the correct framework for animal moral status.').

omega_variable(
    alternative_readings_exclusion_mechanism,
    'Is the exclusion of welfare and abolitionist readings from legal decision-making a consequence of the property reading''s logical structure, or is it enforced suppression external to the reading''s internal coherence?',
    'Analyze institutional gatekeeping mechanisms (how legal reasoning excludes certain arguments, how regulatory agencies frame animal issues, how judicial opinion treats moral status claims). If exclusion follows necessarily from the property axioms, it is structural; if it requires active enforcement and could be reversed by changing framing and institutional rules, it is suppressively enforced.',
    'If structural, the suppression metric reflects necessary costs of maintaining logical consistency. If enforced, the high suppression metric indicates the property reading persists by gatekeeping, not by coherence, and mandatrophy signals become actionable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_exclusion_mechanism, empirical, 'Whether suppression is structural necessity or institutional gatekeeping.').

omega_variable(
    founding_problem_obsolescence,
    'Did the property framework actually solve the founding coordination problem (stable ownership enabling commercial use), or did it simply define away the problem by denying moral status to the entities whose interests were at stake?',
    'Compare the commercial animal-use landscape before and after the property framework was established. If commercial use expanded and stabilized because of the framework, it solved the problem. If commercial use expanded but welfare objections and regulatory pressure persist despite the framework''s suppression infrastructure, the founding problem may have been displaced rather than solved.',
    'If solved: the high extractiveness reflects a legitimate coordination mechanism with extraction as a side effect. If displaced: mandatrophy_resolved should be true, and the constraint persists by institutional inertia and suppression rather than continued legitimation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem is genuinely solved or displaced onto unheard parties.').

omega_variable(
    economic_value_metric_legitimacy,
    'Does restricting moral consideration to economic value represent a defensible normative position, or does it beg the question by stipulating away the very moral status dispute?',
    'Philosophical argument and comparison to other domains: would we accept ''economic value is the only relevant metric'' for human slavery, organ harvesting, or other uses of human beings? If we reject it there but accept it for animals, what is the morally relevant difference? If we cannot articulate a defensible difference, the stipulation may be question-begging.',
    'If economic value is legitimate as the sole metric, the property reading''s structure is coherent. If the restriction is question-begging (assumes what it needs to prove), the reading''s foundation is shakier and vulnerable to challenge from welfare and abolitionist axioms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_value_metric_legitimacy, conceptual, 'Whether the property reading''s restriction of moral considerability to economic value is a defensible axiom or a question-begging stipulation.').

omega_variable(
    consumer_suppression_inertia,
    'Is rising theater ratio (humane slaughter, animal welfare standards) a genuine attempt to improve animal conditions within the property framework, or does it function primarily to manage consumer dissent by creating the appearance of moral concern without altering the property-and-extraction structure?',
    'Measure outcomes: do welfare reforms reduce suffering intensity proportionally, or does their main effect absorb potential consumer objections while extraction intensity remains constant or grows? Do they provide pathways toward abolitionist or welfare-reading endpoints, or do they reinforce the property framework?',
    'If reforms are genuine, theater ratio should decline as satisfaction with welfare conditions rises. If reforms are suppression theater, theater ratio should rise as regulatory volume increases while extraction remains constant—which matches the measurement trajectory. This would indicate mandatrophy dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_suppression_inertia, empirical, 'Whether welfare reforms are genuine amelioration or suppression theater maintaining the property framework against growing resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__property_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__property_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement(anim_tr_t100, animal_status_kernel__property_reading, theater_ratio, 100, 0.16).
narrative_ontology:measurement(anim_tr_t150, animal_status_kernel__property_reading, theater_ratio, 150, 0.17).
narrative_ontology:measurement(anim_tr_t200, animal_status_kernel__property_reading, theater_ratio, 200, 0.18).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__property_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__property_reading, base_extractiveness, 50, 0.85).
narrative_ontology:measurement(anim_be_t100, animal_status_kernel__property_reading, base_extractiveness, 100, 0.88).
narrative_ontology:measurement(anim_be_t150, animal_status_kernel__property_reading, base_extractiveness, 150, 0.9).
narrative_ontology:measurement(anim_be_t200, animal_status_kernel__property_reading, base_extractiveness, 200, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__property_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__property_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(anim_su_t100, animal_status_kernel__property_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(anim_su_t150, animal_status_kernel__property_reading, suppression_requirement, 150, 0.71).
narrative_ontology:measurement(anim_su_t200, animal_status_kernel__property_reading, suppression_requirement, 200, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__property_reading, 0.22).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status_kernel constraint family contains three readings instantiating incompatible normative claims about animal moral status. This story (property_reading) grounds status in property rights and economic value. The welfare_reading grounds it in sentience and suffering, permitting use but constraining it via welfare regulation. The abolitionist_reading grounds it in intrinsic rights and personhood, prohibiting property status categorically. These are not variations on a single constraint—they are three structurally distinct constraints sharing a contested kernel. Each reading has its own ε, beneficiary/victim structure, and classification. The family's structure reflects that animal-status is not a factual question but a normative one: which framework for moral considerability is legitimate. The kernel contest lives in the reading_relations and axioms fields of each story. All three stories share network edges: this reading influences both siblings by defining the permissible moral space (what it means for animals to have status at all); each sibling reading forecloses this reading within its own framework (if welfare or abolitionist axioms are true, the property reading's foundational claim is false). The network models this as mutual influence rather than unilateral foreclosure because no reading has unilateral authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status_kernel__property_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
