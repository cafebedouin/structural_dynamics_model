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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Personhood Boundary: Functional Cognitive Capacity Reading
 *   domain: legal/philosophical
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel 'legal
 *   personhood boundary.' The functional-capacity reading asserts that
 *   personhood should follow from demonstrable cognitive capacity (sentience,
 *   rationality, self-awareness) regardless of species membership. Under this
 *   reading, non-human animals with measurable cognitive abilities (great
 *   apes, cetaceans, corvids) become legal persons with rights, while
 *   non-sentient entities remain property. The constraint extracts from
 *   current property owners and extractive industries (who lose rights over
 *   sentient beings) and benefits identifiable animals and future AI systems
 *   (who gain legal standing). It requires active enforcement because the
 *   reading contradicts centuries of property law and extractive practice;
 *   suppression is high because industries dependent on treating sentient
 *   beings as commodities actively resist it. The constraint is claimed as
 *   tangled_rope: genuine coordination (extending rights protections based on
 *   empirical capacity rather than arbitrary species lines) paired with
 *   asymmetric extraction (stripping rights from powerful payers). The
 *   authored metrics (high suppression, rising extractiveness over time,
 *   moderate theater) describe the actual structural tension of implementing
 *   a rights-expansion reading against institutional and economic resistance.
 *
 * KEY AGENTS:
 *   - Sentient non-human animals (powerless, trapped, beneficiary) — gain legal standing and rights protection under the reading's threshold
 *   - Current property owners of sentient beings (powerful, constrained, payer) — lose property rights in reclassified persons
 *   - Agriculture and extractive industries (organized, constrained, payer) — face operational and profitability restructuring
 *   - Comparative cognition researchers (moderate, mobile, observer) — become arbiters of capacity thresholds
 *   - Jurisdictions adopting the reading (institutional, agenda-setter) — enforce the boundary and develop procedures for non-human person standing
 *   - Human exceptionalist scholars (institutional, constrained, observer) — face delegitimization of humanist interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.72).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Personhood Boundary: Functional Cognitive Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal/philosophical").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, 'b893d448-52a7-4327-8f22-d8d28f3aec0a').
narrative_ontology:cs_kernel_codification('b893d448-52a7-4327-8f22-d8d28f3aec0a', fixed_text).
narrative_ontology:cs_authority_grounding('b893d448-52a7-4327-8f22-d8d28f3aec0a', lineage).
narrative_ontology:cs_interpretation_layer_present('b893d448-52a7-4327-8f22-d8d28f3aec0a').
narrative_ontology:cs_reading_relation('b893d448-52a7-4327-8f22-d8d28f3aec0a', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('b893d448-52a7-4327-8f22-d8d28f3aec0a', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('b893d448-52a7-4327-8f22-d8d28f3aec0a', foundational, cognitive_capacity_species_neutral_personhood).
narrative_ontology:cs_axiom_status(cognitive_capacity_species_neutral_personhood, holdable).
narrative_ontology:cs_axiom_grounding('b893d448-52a7-4327-8f22-d8d28f3aec0a', cognitive_capacity_species_neutral_personhood, empirically_contingent).
narrative_ontology:cs_axiom('b893d448-52a7-4327-8f22-d8d28f3aec0a', foundational, sentience_rationality_self_awareness_sufficient_for_standing).
narrative_ontology:cs_axiom_status(sentience_rationality_self_awareness_sufficient_for_standing, holdable).
narrative_ontology:cs_axiom_grounding('b893d448-52a7-4327-8f22-d8d28f3aec0a', sentience_rationality_self_awareness_sufficient_for_standing, deontological).
narrative_ontology:cs_reference_frame('b893d448-52a7-4327-8f22-d8d28f3aec0a', capacitarian_personhood_framework).
narrative_ontology:cs_drift_state('b893d448-52a7-4327-8f22-d8d28f3aec0a', contemporary_animal_cognition_science_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b893d448-52a7-4327-8f22-d8d28f3aec0a', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, future_artificial_intelligences).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, ecosystems_with_legal_standing).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, current_property_owners_of_sentient_beings).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, agriculture_and_extractive_industries).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, traditional_medical_research_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, environmental_law_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, human_exceptionalist_constitutional_scholars).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, cognitive_capacity_sufficient_for_moral_status).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, species_neutrality_in_rights_attribution).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, sentience_as_legal_threshold).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, animals demonstrating cognitive capacity (great apes, cetaceans, elephants, corvids) gain legal standing as persons with rights rather than property. They cannot exit the legal system but gain protection from conversion to commodities under the constraint's operation. Their trapped status is structural — they are subjects of the law but cannot participate in legal proceedings as agents.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals, beneficiary,
    powerless, biographical, trapped, global).

% Systems achieving the functional-capacity threshold would be admitted to the personhood category, receiving legal protection against termination without due process and potential claims to non-ownership of productive output. They remain analytical stakeholders because no such system yet exists and the reading has not yet encountered actual AI claims.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_artificial_intelligences, beneficiary,
    analytical, civilizational, analytical, universal).

% Owners of legally-recognized sentient animals (zoos, research institutions, agricultural operations, entertainment venues) face a direct loss: animals previously classified as property would be converted to persons, stripping ownership rights and requiring fundamental reorganization of extractive relationships. Exit options are constrained — they cannot exit the legal jurisdiction but can lobby for alternative readings or resist implementation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, current_property_owners_of_sentient_beings, payer,
    powerful, generational, constrained, global).

% Industrial agriculture, pharmaceutical testing, and animal extractive industries bear the constraint as a threat to their fundamental operating model. Reclassifying livestock and laboratory animals as persons would impose duty-of-care costs, restrict breeding and confinement practices, and eliminate the ability to treat sentient beings as fungible inputs. Geographic and jurisdictional constraints limit exit.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, agriculture_and_extractive_industries, payer,
    organized, biographical, constrained, global).

% Biomedical research institutions relying on animal testing face a dual cost: direct (loss of experimental subjects classified as persons), and compliance (shifting from Institutional Animal Care committees to potential criminal liability frameworks). They can relocate research to permissive jurisdictions but cannot exit the legal system itself; those with national regulatory mandates face direct structural conflict.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, traditional_medical_research_establishments, payer,
    institutional, generational, constrained, national).

% Parties arguing for ecosystem legal standing (rivers, forests, species as collectives) align with the functional-capacity reading to the extent that sentience or demonstrated collective cognition could extend personhood to ecosystems themselves. They benefit from the reading's expansion of the personhood boundary but face ambiguity about whether ecosystems meet the cognitive threshold.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, environmental_law_advocates, beneficiary,
    moderate, generational, mobile, global).

% Legal scholars and constitutional interpreters whose authority derives from a humanist reading of personhood face delegitimization under this constraint: their canonical texts and interpretive traditions are reframed as species-biased rather than truth-grounded. They can attempt reinterpretation but cannot exit the hermeneutic authority structure itself.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_exceptionalist_constitutional_scholars, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__functional_capacity_reading, human_exceptionalist_constitutional_scholars, observer).

% Scientists studying animal and machine cognition become arbiters of the personhood boundary: their empirical determinations of cognitive capacity become legally dispositive. They occupy an observer seat because they adjudicate the threshold but do not enforce or directly benefit from its operation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, comparative_cognition_researchers, observer,
    moderate, biographical, mobile, global).

% Courts and legislatures adopting this reading set and enforce it, requiring development of new legal procedures, standing doctrines, and remedies for non-human persons. They may exit by reverting to prior readings but face institutional path-dependence once the framework is adopted — reversals trigger transition costs and legal instability.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, jurisdictions_implementing_functional_capacity_reading, agenda_setter,
    institutional, generational, mobile, regional).

% Sentient beings not yet discovered, whose cognitive capacity cannot yet be demonstrated with available technology (deep-sea organisms, extraterrestrial life if encountered), remain excluded from the conversation despite potential relevance to the boundary. Their exclusion is structural: the reading relies on demonstrable capacity, and demonstration requires recognition and study.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, excluded_future_moral_patients, excluded,
    powerless, civilizational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, current_property_owners_of_sentient_beings).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal framework for recognizing moral patients as legal subjects based on objective cognitive criteria rather than species membership. Solves the coordination problem of how to extend rights protections to non-human sentient beings without abandoning the concept of personhood or creating unlimited claim inflation.
% TRANSFER_FUNCTION: Transfers legal standing and rights protections from the human-exclusively-privileged category to any entity demonstrating cognitive capacity thresholds (sentience, rationality, self-awareness). Transfers liability for harm and duty-of-care obligations from property owners to guardians/caretakers of non-human persons. Transfers property rights in sentient beings from owners to the beings themselves.
% ABSENT_VOICES: Future artificial intelligences cannot testify to their own cognitive status or claim their interests. Non-human animals cannot participate in legal proceedings or advocate for their own boundary-crossing. Sentient beings that fall below currently-measurable cognitive thresholds (organisms with distributed cognition, potential sentience in plants) are structurally excluded from the conversation despite potential relevance.
% DISAPPEARANCE_RATIONALE: If the functional-capacity reading disappeared and personhood reverted to the anthropocentric boundary, property rights in sentient animals would be restored, research and agricultural practices would be reinstated without personhood-based restrictions, and the legal category of non-human persons would dissolve. Industries relying on animal commodification would reorganize toward extraction-friendly practices; jurisdictions that had adopted the reading would face either reversal litigation or constitutional amendment pressure.
% FOUNDING_PROBLEM: Historical legal systems lacked cognitive science and empirical animal cognition research; they defaulted to species membership as a proxy for personhood to minimize complexity. As cognitive science demonstrated sentience and rationality in non-human species (great apes, cetaceans, elephants, corvids, octopuses), the proxy became empirically indefensible and morally arbitrary. The reading emerges to solve the mismatch between what we know animals can do (suffer, reason, form social bonds, plan) and what law permits us to do to them (treat as fungible property).
% FOUNDING_PROBLEM_CORROBORATION: Comparative cognition research (peer-reviewed primatology, cetacean neuroscience, corvid behavior studies) and philosophy of mind publications from outside the extractive industries independently attest the founding problem. Animal welfare advocacy organizations and environmental law scholars corroborate that species-based personhood is empirically undermined. Agricultural and medical research establishments dispute the problem's salience, arguing that functional personhood would destroy beneficial practices. The problem is corroborated by the relevant epistemic communities (animal cognition science, philosophy) but contested by the beneficiary seat (extractive industries) where the corroboration matters most for enforcement.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).

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
 *   Extractiveness is authored at 0.68 because the reading directly converts value from one seat (property owners, industries) to another (animals, their advocates) through legal reclassification rather than through compensation or graduated transition. The conversion is asymmetric: no offsetting benefit flows to payers; the coordination (rights framework) benefits are distributed to beneficiaries without offsetting payer participation. Suppression is high (0.72) because the reading contradicts established property law and economic practice across agriculture, research, and entertainment — institutions must actively exclude and criminalize this reading to prevent adoption. Theater rises from 0.28 to 0.41 over the interval because as the reading gains scientific support (animal cognition research accumulates), institutional defenses increasingly rely on rhetorical barriers (claims of 'practical impossibility,' economic catastrophism) rather than substantive legal arguments, and enforcement becomes performative — courts pretend personhood-for-animals cannot be squared with existing doctrine rather than engaging the doctrinal reinterpretation the reading demands. Accessibility collapse is moderate (0.62) because the reading does not eliminate alternative frameworks; the restrictive anthropocentric and developmental-potentiality readings remain available and institutionally entrenched, so the functional-capacity reading is contestable rather than inevitable. Resistance is high (0.74) because the payers and extractive industries actively oppose the reading through litigation, legislative lobbying, and institutional sabotage of cognitive capacity determinations.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of current property owners and extractive industries, this constraint is pure extraction with no coordination benefit — it transfers rights they possess to animals without compensation or transition mechanism. From the seat of non-human animals and their advocates, the constraint is genuine coordination: it establishes a principled, non-arbitrary framework (capacity-based rather than species-based) for recognizing moral standing. From the observer seat (comparative cognition research), the reading is an empirical application of what animal consciousness science actually shows. These perspectives are structurally incompatible because they disagree on what the 'real' function of the constraint is: rights-transfer vs. rights-recognition. The engine computes directionality from each seat's relationship to the constraint — for payers it will be high (near 1.0, target), for beneficiary animals it will be low (subsidized), for observers it will be near 0.5 (symmetric). The perspectival divergence is the point: the claim and the metrics do not reconcile because different seats do not agree on what is being coordinated or extracted.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and extractive industries face high directionality (near 1.0, targets) because the reading strips their legal rights over sentient beings without compensation or exit route. Their exit options are constrained (cannot leave the jurisdiction without abandoning operations) and their power status does not protect them against a jurisdictional boundary change. Sentient non-human animals have low directionality (near 0.0, beneficiaries) because the reading grants them rights and protection — though they are trapped (cannot exit the jurisdiction), the constraint subsidizes their legal standing. The animals have no power to enforce the reading themselves, but they are beneficiaries of it nonetheless. Comparative cognition researchers have near-zero suppression impact (mobility, analytical status) so their directionality is analytical (not scaled by extraction). Human exceptionalist scholars have moderate-to-high directionality (their authority is delegitimized, but they retain institutional position and can resist through reinterpretation) — they are targets of the reading's challenge but not fully exposed like property owners.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (empirical mismatch between what we know animals can do and what law permits us to do to them) remains live, and the reading's coordination function (capacity-based personhood) is still needed. However, the extracted-from seat (property owners, industries) actively maintains the alternative reading (anthropocentric personhood) to preserve their rent-extraction capacity. The constraint does not resolve to mandatrophy because the founding problem's salience is contested — extractive industries claim the problem is solved by existing animal welfare law (which permits harm as long as it is 'regulated'), while advocates claim welfare law is cover for continued extraction. The reading avoids mandatrophy diagnosis precisely because this contest is live: the constraint persists because it functions for one seat (enables profitable extraction under property law) even as another seat experiences it as harmful (animals lack rights). Piton diagnosis is avoided because the payer seats collect substantial extraction value (property rights, research access, agricultural profit), so the constraint is not maintained by inertia but by active benefit capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_threshold_operationalization,
    'How is ''demonstrable cognitive capacity'' operationalized, and who decides? Is the threshold sentience-only, or does it require rationality or self-awareness? How do disagreements about capacity attribution get resolved?',
    'Courts or legislatures establish cognitive thresholds through precedent or statute; comparative cognition science provides evidence; comparative jurisprudence from adopting jurisdictions reveals de facto standards and disputes.',
    'A stringent threshold (consciousness + self-recognition) would narrow the beneficiary set; a lax threshold (any nociception) would expand it dramatically. The enforceability of the reading depends on an operationalizable standard, and different thresholds produce different extraction patterns for payers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_capacity_threshold_operationalization, conceptual, 'The operationalization ambiguity in ''demonstrable cognitive capacity'' — who decides and what counts as demonstration.').

omega_variable(
    kernel_reading_contest_anthropocentrism_vs_capacity,
    'Is the anthropocentric boundary (personhood = human species membership) a genuine foundational axiom of Western legal tradition, or a pragmatic proxy that becomes indefensible as empirical animal cognition is demonstrated?',
    'Historical jurisprudence analysis; contemporary legal theory decomposing the arguments for and against anthropocentrism; empirical testing: does adoption of the functional-capacity reading collapse or evolve existing constitutional frameworks?',
    'If anthropocentrism is foundational, adopting this reading requires rewriting constitutional personhood doctrine at its core — a severe structural challenge to jurisdictions claiming to preserve continuity. If it is a pragmatic proxy, the reading presents as evolution rather than revolution, reducing institutional resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_anthropocentrism_vs_capacity, conceptual, 'Whether human exceptionalism is a foundational legal axiom or a contingent proxy.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Does suppression of this reading arise from legal doctrine (structural barriers to reinterpretation) or from internalized identity fusion among legal scholars (belief that personhood=humanity is self-evident)?',
    'Jurisdictions that adopt the reading serve as a test: if adoption occurs and legal professionals rapidly shift interpretation (showing the doctrine was plastic), suppression was largely internalized; if resistance persists through formal channels and legal institutions refuse reinterpretation, suppression is structural.',
    'If suppression is internalized, it persists even after formal barriers lift — scholars and judges trained in anthropocentrism would resist despite legal permission. If structural, removing formal barriers (legislative override, new constitutional language) would enable rapid adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of the functional-capacity reading is structural or internalized in legal culture.').

omega_variable(
    artificial_intelligence_applicability_ambiguity,
    'Does the functional-capacity reading apply to artificial systems, or is there an implicit boundary at ''biological sentience''? If AI systems achieve measurable cognitive capacity, are they persons under this reading?',
    'The reading''s own axioms settle this: if cognition regardless of substrate is the criterion, AI inclusion is forced; if an implicit biological substrate is assumed, the reading reverts to a different boundary. Testing comes when AI systems pass the threshold and claim standing.',
    'If AI inclusion is forced, the beneficiary set is potentially unbounded and extractiveness increases (AI developers lose control-rights over systems). If biological substrate is implicit, the reading narrows and cognitive-capacity advocates face an inconsistency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(artificial_intelligence_applicability_ambiguity, conceptual, 'Whether functional cognitive capacity regardless of substrate includes artificial systems.').

omega_variable(
    ecosystem_collective_cognition_boundary,
    'Can ecosystems, species collectives, or superorganisms (ant colonies, fungal networks) meet the cognitive-capacity threshold, and if so, what does legal personhood mean for a non-agent collective?',
    'Comparative cognition science on collective behavior and distributed intelligence; legal theory on how to assign rights and duties to non-agent entities; natural experiments from jurisdictions extending legal standing to ecosystems.',
    'If collectives can be persons, the beneficiary set expands to environmental systems and the extractiveness of property rights in land, water, and biotic communities increases dramatically. If only individual-agent cognition counts, ecosystems remain property regardless of demonstrated collective intelligence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecosystem_collective_cognition_boundary, conceptual, 'Whether collective cognition (ecosystems, species swarms) qualifies for personhood under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(lega_tr_t0, observed).
narrative_ontology:measurement(lega_tr_t5, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(lega_tr_t5, observed).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(lega_tr_t10, observed).
narrative_ontology:measurement(lega_tr_t15, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(lega_tr_t15, observed).
narrative_ontology:measurement(lega_tr_t25, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(lega_tr_t25, projected).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(lega_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(lega_be_t0, observed).
narrative_ontology:measurement(lega_be_t5, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(lega_be_t5, observed).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(lega_be_t10, observed).
narrative_ontology:measurement(lega_be_t15, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(lega_be_t15, observed).
narrative_ontology:measurement(lega_be_t25, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(lega_be_t25, projected).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(lega_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(lega_su_t0, observed).
narrative_ontology:measurement(lega_su_t5, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(lega_su_t5, observed).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(lega_su_t10, observed).
narrative_ontology:measurement(lega_su_t15, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(lega_su_t15, observed).
narrative_ontology:measurement(lega_su_t25, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(lega_su_t25, projected).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(lega_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__functional_capacity_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% The legal_personhood_boundary kernel decomposes into three constraint stories, each representing a different reading of what grounds personhood in law. The functional_capacity_reading is decomposed as a separate constraint because its ε value (0.68 extractiveness from property owners) and structural beneficiary set (animals, future AI) differ materially from the restrictive_anthropocentric_reading (lower extractiveness for property owners, no AI beneficiaries) and the developmental_potentiality_reading (different victim set: abortion access instead of property rights). The epsilon-invariance principle requires separate stories: measuring personhood via species membership yields different extraction profiles than measuring it via cognitive capacity, because the scope of who loses/gains rights differs. All three stories link via network.affects_constraints to document the kernel decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
