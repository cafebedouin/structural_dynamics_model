% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Animal Rights – Abolitionist Reading
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint embodies the abolitionist reading of animal moral status:
 *   animals possess inherent, inviolable moral standing that precluding all
 *   instrumental use—food production, research, entertainment, labor. This is
 *   ONE reading of a contested kernel (animal_status kernel) with two
 *   structural siblings: the property reading (animals as legal objects
 *   without independent moral standing) and the welfare reading (animals as
 *   sentient beings whose interests constrain but do not prohibit human use).
 *   The abolitionist reading is presented here as establishing a natural
 *   moral law—that inherent animal dignity is a fact about reality,
 *   discoverable through moral reasoning—while simultaneously benefiting
 *   identifiable advocates (philosophers, activists, advocacy organizations)
 *   who gain institutional and material authority from the reading's
 *   adoption. This tension (natural law vs. beneficiary-maintained
 *   construction) triggers false-summit evaluation. The claim and metrics are
 *   independently authored: claimed as mountain (natural moral law), measured
 *   with zero extractiveness (the reading imposes no wrongful extraction ON
 *   animals—it vindicates their status) but very high suppression
 *   (enforcement against current institutional practitioners requires
 *   suppressing their power and reshaping their incentives). Suppression is
 *   rising over the measurement interval (0.85 to 0.92), reflecting
 *   increasing cultural and legal pressure against animal instrumental use,
 *   particularly in jurisdictions adopting animal-rights frameworks.
 *
 * KEY AGENTS:
 *   - Nonhuman animals: powerless, trapped, victims of current instrumental-use regimes; designated as rights-holders by the abolitionist reading
 *   - Abolitionist advocates: organized, mobile, beneficiaries of the reading's authority and institutionalization; hold the moral and policy framework
 *   - Agricultural extractors: powerful, constrained, excluded from the abolitionist conversation; their current justifications would be delegitimized
 *   - Research institutions: powerful, constrained, excluded; animal research is constituted as rights violation under the reading
 *   - Moral philosophy tradition: non-agent; vindicated by adoption of the abolitionist axiom; gains coherence from institutional arrangements recognizing it
 *   - Welfare tradition: non-agent, competing; read as structural barrier to moral progress rather than legitimate alternative
 *   - Consumers: organized, excluded; their material and cultural benefits are constituted as illicit under the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.0).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.92).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, mountain).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Rights – Abolitionist Reading").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:emerges_naturally(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '2492c630-1a5f-4a88-bcfc-849951761c6a').
narrative_ontology:cs_kernel_codification('2492c630-1a5f-4a88-bcfc-849951761c6a', distributed).
narrative_ontology:cs_authority_grounding('2492c630-1a5f-4a88-bcfc-849951761c6a', distributed).
narrative_ontology:cs_reading_relation('2492c630-1a5f-4a88-bcfc-849951761c6a', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('2492c630-1a5f-4a88-bcfc-849951761c6a', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('2492c630-1a5f-4a88-bcfc-849951761c6a', foundational, animals_possess_inviolable_moral_status).
narrative_ontology:cs_axiom_status(animals_possess_inviolable_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('2492c630-1a5f-4a88-bcfc-849951761c6a', animals_possess_inviolable_moral_status, deontological).
narrative_ontology:cs_axiom('2492c630-1a5f-4a88-bcfc-849951761c6a', foundational, instrumental_use_violates_rights).
narrative_ontology:cs_axiom_status(instrumental_use_violates_rights, holdable).
narrative_ontology:cs_axiom_grounding('2492c630-1a5f-4a88-bcfc-849951761c6a', instrumental_use_violates_rights, deontological).
narrative_ontology:cs_reference_frame('2492c630-1a5f-4a88-bcfc-849951761c6a', animals_as_moral_patients).
narrative_ontology:cs_drift_state('2492c630-1a5f-4a88-bcfc-849951761c6a', contemporary_jurisdictional_divergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2492c630-1a5f-4a88-bcfc-849951761c6a', '2026-06-13T14:32:00Z').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, moral_philosophy_tradition).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, abolitionist_advocates).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, nonhuman_animals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the abolitionist reading, animals are rights-holders with inherent moral standing; any instrumental use (food production, research, entertainment, labor) violates that standing and constitutes wrongful extraction. They cannot exit or consent; they are wholly captive to human institutional choices about their use. The abolitionist frame constitutes them as victims of any extractive arrangement.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, nonhuman_animals, payer,
    powerless, biographical, trapped, global).

% Advance a moral and political position that animals deserve inviolable rights and that human institutions should be reorganized around that principle. They hold the moral and policy framework; they do not extract material benefit from animals themselves. Their stake is in the legitimacy and dominance of the abolitionist reading over competitor readings.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, abolitionist_advocates, observer).

% Currently organize production systems on the premise that animals are property without rights. The abolitionist reading excludes them from the conversation about animal moral status; it does not grant them a voice in defining animal standing. Their current institutional arrangements would be delegitimized if the abolitionist reading won cultural/legal adoption.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, agricultural_extractors, excluded,
    powerful, biographical, constrained, global).

% Conduct experiments on animals under the institutional premise that animals are tools for knowledge production. The abolitionist reading constitutes such use as rights violation. They are structurally excluded because acceptance of the reading would eliminate their current justification for animal research.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, research_institutions, excluded,
    powerful, biographical, constrained, global).

% Benefit materially and culturally from animal-derived products and services (food, clothing, entertainment, medical advances from research). The abolitionist reading constitutes these benefits as illicit—gained through wrongful extraction from beings with inviolable rights. They are excluded because the reading does not recognize their consumption interests as morally legitimate.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_benefiting_from_use, excluded,
    organized, biographical, constrained, global).

% Analyze the coherence and implications of the abolitionist reading against competitor readings (property, welfare) and examine its implications for legal doctrine. Their seat is analytical; they do not advocate for a particular reading but model its structure and consequences.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_philosophers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The abolitionist reading does not coordinate action among parties who benefit mutually—it establishes a unilateral moral claim: animals have rights that preclude all instrumental use. There is no coordination problem being solved; there is a moral boundary being asserted. No coordination function exists within this reading.
% TRANSFER_FUNCTION: Under the abolitionist frame, no legitimate transfer occurs—all current instrumental use of animals constitutes wrongful extraction. Food production transfers animal bodies to human consumption; research transfers animal capacity for suffering to human knowledge gain; entertainment transfers animal autonomy to human pleasure. All are constituted as moral violations, not legitimate transfers.
% ABSENT_VOICES: Agricultural producers, research institutions, and consumers benefiting from animal use are structurally excluded from the conversation. They would argue that animal use is justified by human interests, evolutionary hierarchy, or welfare accommodations—but the abolitionist reading does not admit these arguments as legitimate. Also absent: animals themselves, who cannot speak to the framework and would not consent to its premises if they could (though the reading claims to speak for their interests).
% DISAPPEARANCE_RATIONALE: If the abolitionist reading's moral authority disappeared and animals were redefined as moral resources without inherent standing, human institutions would rapidly reorganize toward expanded instrumental use. Conversely, if the reading's authority became dominant and binding on law, the agricultural, research, and food systems would face radical restructuring or collapse. The world's institutional organization directly depends on which reading of animal status prevails.
% FOUNDING_PROBLEM: How should moral status be distributed among sentient beings? Specifically: do animals have intrinsic moral value that constrains or prohibits their use as means to human ends, or is their moral status derivative of human interests and purposes?
% FOUNDING_PROBLEM_CORROBORATION: Philosophers and animal advocates outside the user-benefiting industries attest the problem is live and urgent: Peter Singer, Tom Regan, Martha Nussbaum, and contemporary animal rights scholars argue the foundational question remains unresolved in philosophy and law. Agricultural and research institutions dispute this characterization, claiming animal welfare frameworks have adequately settled the question—but their denial of the problem is itself an answer within the debate, not an independent corroboration. External academic philosophers, ethicists, and legal scholars confirm the founding problem persists as contested.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status__abolitionist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status__abolitionist_reading),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero (0.0) across the measurement interval because the abolitionist reading does not extract from any party—it assigns moral status to animals and directs that instrumental use cease. The reading is not a mechanism of extraction; it is a moral claim that delegitimizes extraction. Theater ratio is zero (0.0) because there is no gap between the reading's stated function (establishing animal rights, stopping instrumental use) and its actual operation (it does establish and defend the claim). Suppression is very high and rising (0.85 to 0.92 over the interval) because enforcement of the reading requires active suppression of competing institutional arrangements: agricultural systems, research programs, consumer behavior, and the cultural normalization of animal use. The rising trajectory reflects increasing cultural and legal pressure against these arrangements, particularly in jurisdictions adopting animal-rights frameworks. The accessibility of alternatives to animal instrumental use is collapsing as the reading gains adoption (0.88): the cultural and economic pathways to veganism, plant-based research, and animal-free entertainment become more accessible and normalized, which paradoxically makes the continued use of animals appear more difficult to justify, not because alternatives are physically unavailable but because the moral framework (the reading itself) makes use seem categorically wrong. Resistance to the reading is moderate (0.55): substantial institutional and consumer opposition exists (agricultural, research, cultural), but the reading has achieved enough adoption in some jurisdictions and intellectual communities to generate sustained momentum.
 *
 * PERSPECTIVAL GAP:
 *   From the abolitionist advocate seat: the reading is a discovery of moral truth—animals DO have inherent standing, and instrumental use IS wrongful extraction. The suppression is justified enforcement of a moral boundary. From the agricultural/research seat: the reading is an illegitimate moral claim that ignores human interests and the benefits of animal use; the suppression is coercive imposition of ideology. From the animal seat: the reading offers vindication and rights recognition, but does not change the current material reality of instrumental use in most jurisdictions. From the philosophical/analytical seat: the reading is one live option among three structurally distinct competitors, each with internal coherence. The engine computes these divergences from the stakeholder power levels, exit options, and directionality: agricultural institutions have powerful seats with constrained exit; advocates have organized seats with mobile exit; animals are powerless and trapped; philosophy has analytical seats with no material stakes. Each seat's experience of the constraint diverges because their structural relationship to the reading's authority differs.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has a degenerate directionality structure because it is a moral reading, not a mechanism that extracts or coordinates material resources. The abolitionist reading does not have beneficiaries in the classical sense (parties who collect rents from its operation). Instead, it has advocates (philosophers, activists) who benefit from the reading's institutional authority and status. Animals are not victims OF the abolitionist reading; they are vindicated BY it. They are victims of the current instrumental-use regime that the reading opposes. The suppression is directed against agricultural, research, and consumer institutions—not against animals. Abolitionist advocates benefit from institutional recognition and legal adoption of their reading but do not extract material resources from animals via the reading itself. The suppression manifests as: (1) exclusion of competing readings from legitimate policy discourse, (2) delegitimation of user-benefiting institutions, (3) normalization of animal-rights frameworks in law and education, (4) cultural pressure on consumer and institutional practices. This is structural pressure on institutional arrangements, not extraction from individual animals. The constraint is better understood as establishing a moral boundary than as extracting material wealth. The engine's directionality computation will reflect this: abolitionist_advocates sit at d near beneficiary (they advocate, they gain authority, but they do not extract material from animals); agricultural_extractors sit at d near target (their institutions are delegitimized, their practices are suppressed); nonhuman_animals sit at d near target under current regimes but would shift toward beneficiary/vindicated under full adoption of the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is present in the classical sense: the abolitionist reading's mandate (animals have inviolable rights; instrumental use is wrong) has not outlived its function. The founding problem (how to distribute moral status among sentient beings) remains live and contested. The reading persists because advocates actively maintain it and because cultural/legal momentum toward animal rights has gained substantial force in some jurisdictions. There is no zombie constraint here—only a live moral and political contest. If the abolitionist reading were to become legally dominant and institutionally settled (e.g., if animal rights were recognized in law globally and animal-use industries were prohibited), then mandatrophy might eventually emerge: the constraint persists but the founding problem is 'settled' and the advocate class might shift to other causes. That future state is not yet present. Current state: live mandate, live contest, rising institutional pressure toward adoption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_of_moral_status_claim,
    'Is the abolitionist reading''s claim that animals possess inherent moral status a natural law—a fact about the structure of morality independent of human choice—or a constructed moral commitment that benefits human advocates who hold it?',
    'This is a conceptual/preference-class question with no empirical resolution. The ambiguity arises from the reading''s presentation as discovering a natural fact (animals'' inherent worth) versus positing a moral commitment (we CHOOSE to recognize them as bearers of rights). Distinguish: (1) empirical facts about animal cognition and suffering (resolvable by neuroscience) from (2) the moral inference that such facts ground inviolable rights (not resolvable by evidence alone). The reading treats the inference as necessary; competitors treat it as choice.',
    'If the claim is natural law, alternative readings (property, welfare) are moral errors to be corrected. If the claim is constructed commitment, the three readings represent competing legitimacy frameworks with no ultimate fact adjudicating between them. FSM evaluation: abolitionist advocates (moral_philosophy_tradition, abolitionist_advocates) benefit from establishing their reading as natural law; this benefits them materially (institutional authority) and epistemically (their view is vindicated). The false-summit mechanism should flag this story as a candidate for reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturality_of_moral_status_claim, conceptual, 'Whether animal moral status is discovered natural law or constructed moral commitment with beneficiaries.').

omega_variable(
    victims_versus_vindicated_propositions,
    'Are nonhuman animals listed as victims because they bear genuine wrongful costs under the abolitionist reading, or are they better understood as a class whose interests the reading vindicates rather than a class that suffers from the reading''s operation?',
    'The abolitionist reading does not extract from animals—it assigns them rights and stops instrumental use. The ''victimization'' is of the CURRENT system (in which animals are used), not of the abolitionist reading itself. Animals are vindicated (assigned moral status) rather than victimized by the reading. This is an authoring classification question: should animals appear in base_properties.victims (denoting wrongful cost borne) or base_properties.vindicated_propositions (denoting interests championed)? The prompt directs ''Animals fully in victim set for any instrumental use''—interpreted as: the reading designates animals as victims of the current instrumental-use regime and beneficiaries of rights recognition via the abolitionist frame. Under this reading, animals are vindicated by the abolitionist claim; they do not bear costs FROM it.',
    'If reclassified to vindicated_propositions (the reading vindicates the claim that animals are moral patients), the constraint structure shifts: no victim group, extractiveness remains 0.0 (the reading imposes no extraction), suppression remains high (enforcement of the reading against current practitioners requires suppressing their institutional power). The constraint remains mountain-shaped but with different conceptual grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victims_versus_vindicated_propositions, conceptual, 'Whether animals are victims of the abolitionist reading or vindicated by it.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the abolitionist reading''s core axiom (animals possess inviolable moral status) logically foreclose the property and welfare readings, or do all three readings coexist as live but incompatible positions held by different parties?',
    'Logical foreclosure would require: acceptance of the abolitionist axiom necessarily entails rejection of any premise the property or welfare reading rests on. The property reading asserts animals are legal objects without independent moral standing; the abolitionist reading directly contradicts this. Within a single coherent framework (e.g., a legal system), both cannot be true simultaneously. However, different legal jurisdictions, cultural traditions, and philosophical schools DO hold all three simultaneously. The question is whether they are logically incommensurable (foreclosing) or merely competing (coexisting). This determines the cs_structure.reading_relations choice.',
    'If forecloses: the abolitionist reading is positioned as the correction of a fundamental error (property reading) and the supersession of an inadequate compromise (welfare reading). If coexists_with: the three readings are permanent alternatives whose adoption depends on cultural/legal/philosophical choice, not logical necessity. The engine uses reading_relations to compute compatibility; the right choice shapes downstream contamination analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the abolitionist reading logically forecloses its siblings or coexists with them as permanent alternatives.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the high measured suppression (0.92) structural—external enforcement machinery preventing animal advocates from reshaping institutions—or internalized—the normalization of animal instrumental use so complete that alternatives are psychologically unthinkable to many, even without active enforcement?',
    'Post-exposure suppression trajectory: if exposure to abolitionist arguments and legal change opportunities (e.g., jurisdictional shift toward animal rights law) causes rapid adoption of the reading, suppression was primarily structural and externally maintained. If resistance persists despite exposure and opportunity, suppression is partially internalized (the use relationship is normalized as inevitable or natural). Cultural and legal variation across jurisdictions provides a natural experiment: regions with stronger animal-rights legal frameworks show higher adoption of the abolitionist reading; this suggests structural suppression (law and institutional incentives, not internal conviction).',
    'If suppression is primarily structural, legal and institutional change can shift the reading''s dominance relatively quickly. If partially internalized, changing minds requires long-term cultural education beyond legal reform. The constraint''s lifecycle trajectory differs: structural suppression predicts faster phase transitions with legal change; internalized suppression predicts longer resistance even after legal openings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether high suppression is structural enforcement or internalized normalization of animal use.').

omega_variable(
    extractive_beneficiary_identity_of_tradition,
    'Does the abolitionist philosophical tradition benefit materially and institutionally from being adopted as law/policy, making it a potential false summit (a reading that appears natural/principled but whose persistence depends on beneficiaries maintaining it)?',
    'Examine the career incentives and institutional positioning of abolitionist advocates. Do academic philosophers gain publication, funding, and status from championing the reading? Do activist organizations gain donor support and political power? Do policy institutions gain legitimacy from adopting it? The tradition itself is not an agent and does not extract; but the agents who hold and advocate for it DO extract institutional and material benefit from the reading''s dominance. This is distinct from the reading''s moral truth: a reading can be both true AND benefit its advocates. However, the false-summit mechanism requires an omega documenting the beneficiary structure when it exists.',
    'Identifying beneficiaries (abolitionist_advocates, moral_philosophy_tradition) does not invalidate the reading''s claims but flags the need for independent corroboration of the founding problem from non-interested parties. The reading''s moral authority should rest on argument, not on advocacy incentives. This omega routes through the FSM gate: a mountain with declared beneficiaries requires omegas documenting the natural-law vs. constructed ambiguity—exactly what this omega addresses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractive_beneficiary_identity_of_tradition, empirical, 'Whether abolitionist advocates extract material/institutional benefit from the reading''s adoption, creating false-summit risk.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t25, animal_status__abolitionist_reading, theater_ratio, 25, 0.0).
narrative_ontology:measurement_basis(anim_tr_t25, observed).
narrative_ontology:measurement(anim_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.0).
narrative_ontology:measurement_basis(anim_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t25, animal_status__abolitionist_reading, base_extractiveness, 25, 0.0).
narrative_ontology:measurement_basis(anim_be_t25, observed).
narrative_ontology:measurement(anim_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.0).
narrative_ontology:measurement_basis(anim_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t25, animal_status__abolitionist_reading, suppression_requirement, 25, 0.9).
narrative_ontology:measurement_basis(anim_su_t25, observed).
narrative_ontology:measurement(anim_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.92).
narrative_ontology:measurement_basis(anim_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status__abolitionist_reading, 0.12).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel admits three structurally distinct readings, each with different ε-values, beneficiary/victim structures, and institutional implications. The abolitionist reading (this story) asserts zero extractiveness and high suppression, establishing animal rights as a natural moral law. The property reading treats animals as legal objects, admitting extractive use with welfare constraints. The welfare reading permits regulated use that respects animal interests. These are NOT observations of the same constraint from different angles—they are different constraints instantiated by the same kernel under different readings. Each story is ε-invariant and independently classified. The three stories are linked via network.affects_constraints to indicate their common origin in the kernel contest and to enable contamination analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
