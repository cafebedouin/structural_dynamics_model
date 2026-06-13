% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Animal Property Status as Categorical Rights Violation (Abolitionist Reading)
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   The abolitionist reading of the animal status kernel holds that animals
 *   are moral persons with inherent rights against being owned or used,
 *   regardless of how humanely that use is conducted. This reading classifies
 *   the entire property regime — from factory farming to companion animal
 *   ownership to research use — as extractive violation. The constraint maps
 *   perfectly to a snare: animals are victims with zero exit options, the
 *   regime requires active enforcement (law, police, market mechanisms), and
 *   the suppression is both structural (legal barriers) and internalized
 *   (cultural narratives that naturalize animal property). The reading
 *   differs from welfare and property readings in its categorical opposition
 *   to use itself, not merely its conditions. Theater is relatively low
 *   (0.22–0.25) because the abolitionist position offers no regulatory cover
 *   story — it must explicitly name the entire regime as injustice.
 *   Extractiveness is consistently high (0.88–0.92) because under this
 *   reading, ANY use of animals as property violates their fundamental
 *   rights; marginal welfare improvements do not reduce the extractiveness,
 *   they obscure it.
 *
 * KEY AGENTS:
 *   - non_human_animals: structurally powerless, trapped, victims of the property regime with no negotiating position
 *   - animal_agriculture_industries: institutional agenda-setters who administer and benefit from the property regime; have exit options (can transform business models) but prefer enforcement status quo
 *   - medical_and_research_establishments: institutional agenda-setters who use animals as research property; face incremental regulation but not categorical prohibition under current regime
 *   - legal_property_frameworks: the doctrinal system that naturalizes animal property; non-agent but agency-enabling
 *   - abolitionist_advocates: moderate-power payers who bear costs of challenging the regime; their resistance is organized but faces entrenched institutional opposition
 *   - welfare_reformers: excluded from the conversation as framed by abolitionists; they would object that incremental change is pragmatically possible and morally necessary
 *   - consumer_publics: organized payers who benefit from cheap animal products and bear moral cost of the system; have constrained exit options
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.92).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.78).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Animal Property Status as Categorical Rights Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '811ba0a6-17d8-445e-a20d-668b30f7cf09').
narrative_ontology:cs_kernel_codification('811ba0a6-17d8-445e-a20d-668b30f7cf09', distributed).
narrative_ontology:cs_authority_grounding('811ba0a6-17d8-445e-a20d-668b30f7cf09', distributed).
narrative_ontology:cs_reading_relation('811ba0a6-17d8-445e-a20d-668b30f7cf09', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('811ba0a6-17d8-445e-a20d-668b30f7cf09', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('811ba0a6-17d8-445e-a20d-668b30f7cf09', foundational, animal_moral_personhood_inherent).
narrative_ontology:cs_axiom_status(animal_moral_personhood_inherent, holdable).
narrative_ontology:cs_axiom_grounding('811ba0a6-17d8-445e-a20d-668b30f7cf09', animal_moral_personhood_inherent, deontological).
narrative_ontology:cs_axiom('811ba0a6-17d8-445e-a20d-668b30f7cf09', foundational, categorical_abolition_imperative).
narrative_ontology:cs_axiom_status(categorical_abolition_imperative, holdable).
narrative_ontology:cs_axiom_grounding('811ba0a6-17d8-445e-a20d-668b30f7cf09', categorical_abolition_imperative, deontological).
narrative_ontology:cs_reference_frame('811ba0a6-17d8-445e-a20d-668b30f7cf09', animals_as_rights_bearers).
narrative_ontology:cs_drift_state('811ba0a6-17d8-445e-a20d-668b30f7cf09', contemporary_property_regime_persistence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('811ba0a6-17d8-445e-a20d-668b30f7cf09', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, non_human_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_agriculture_industries).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, medical_and_research_establishments).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, consumer_publics).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, abolitionist_advocates).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, consumer_publics).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, animal_moral_personhood).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, categorical_abolition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Treated as property under law and social convention. Subject to ownership, use, confinement, breeding selection, killing, and exploitation across economic sectors (agriculture, research, entertainment, clothing). Under the abolitionist reading, their status as property IS the fundamental injustice; welfare improvements do not remedy this, only perpetuate it by making the violation seem acceptable. Exit is structurally impossible — they cannot refuse, negotiate, or escape the property regime without human intervention on the scale of abolition itself.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, non_human_animals, payer,
    powerless, immediate, trapped, global).

% Administers and enforces animal property status across production systems. Directly benefits from the legal designation of animals as property — it enables capture of reproductive capacity, labor, bodies, and byproducts as commodities. The economic model is predicated on treating animals as property; abolition would require complete structural transformation or exit from the sector. Currently can shift production methods (welfare reforms) without losing the property framework or primary revenue streams.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_agriculture_industries, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, animal_agriculture_industries, beneficiary).

% Uses animals as property for research, testing, and model systems. Defends this use as necessary for human medical advancement and product safety. Benefits from unrestricted access to animal subjects at low legal and reputational cost. Welfare regulations and replacement technologies (in vitro, computational models) create friction, but property status eliminates the more fundamental challenge — the right NOT to be used at all. Abolition would force substitution of all animal research, a constrained but navigable transition.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, medical_and_research_establishments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, medical_and_research_establishments, beneficiary).

% The doctrinal system treating animals as things, not beings with rights. Benefits from this classification because it enables a coherent legal system where ownership claims are absolute and inalienable within the owner's domain. Abolition would require reconstituting the boundary between property and persons — a foundational legal redefinition with spillover effects across contract, tort, inheritance, and criminal law frameworks.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_property_frameworks, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__abolitionist_reading, legal_property_frameworks).

% Argue that animal property status is fundamentally unjust and all use is impermissible. Bear the costs of advocating against entrenched legal and economic interests — litigation costs, reputational risk, institutional exclusion, slow legislative progress. Have exit options (can shift advocacy focus or leave the movement) but sustained commitment to this reading creates identity-fusion with the abolitionist position, narrowing strategic flexibility.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, payer,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, abolitionist_advocates, observer).

% Advocate for incremental improvements to animal treatment while retaining property status. Under the abolitionist reading, this is a structural error — welfare reforms legitimize and perpetuate the property status by suggesting it can be made acceptable. Reformers would object that incremental progress is pragmatically necessary and that abolition is a distant horizon; abolitionists counter that every welfare measure extends the social license for property treatment and delays the categorical shift. Both read the same legal-doctrinal kernel; their strategic disagreement is whether regulation or abolition is the path to rights-based protection.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reformers, excluded,
    moderate, generational, mobile, global).

% Benefit from cheap animal products and research outcomes enabled by treating animals as property. Pay indirectly by living in a world where this property status is maintained — both through the economic extraction of animals and through the moral costs of a system premised on denying the personhood of billions of sentient beings. Exit options exist (dietary and lifestyle change) but are constrained by infrastructure, cultural embedding, and lack of awareness of the dependency.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, consumer_publics, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__abolitionist_reading, consumer_publics, beneficiary).

% Adjudicate and enact law that treats animals as property. Face pressure from both abolitionist advocates (rights-based arguments) and industry (economic arguments). The status quo requires active maintenance — courts must enforce the property regime through tort doctrine (animals as damages, not victims), criminal law (property damage not assault), and contract law (owner discretion). Theoretically can reframe animals as rights-holders, but this requires overcoming doctrinal inertia and redistributing entrenched benefits.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, judicial_and_legislative_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Frameworks (Cartesian dualism, utilitarian cost-benefit, property law doctrine) that naturalize animal property status as either inevitable (nature) or rationally justified (economic/philosophical grounds). These traditions make the property regime appear to be a discovery rather than a construction. Abolition requires displacing these frameworks or reinterpreting them — a slow, high-friction epistemic transition.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, dominant_philosophical_traditions, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(animal_status_kernel__abolitionist_reading, dominant_philosophical_traditions).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The abolitionist reading rejects any coordination narrative. What appears to be coordination (efficient allocation of animal bodies for human use) is reframed as foundational injustice enabled by denying animals' moral status. The constraint serves no coordination function; it serves pure extraction.
% TRANSFER_FUNCTION: Transfers animal bodies, labor, reproductive capacity, and lives from animals (who have no legal standing to refuse) to human economic and research beneficiaries (industries, researchers, consumers). The transfer is coercive and unidirectional: animals cannot negotiate, consent, or exit.
% ABSENT_VOICES: Animals themselves are structurally absent from the conversation. Their interests can only be proxied by human advocates. This structural silence is the enabling mechanism of the property regime — if animals could testify, the regime would face legitimacy collapse. Welfare reformers are present but would contest this reading, arguing that regulation and abolition are not mutually exclusive and that incremental progress is both morally necessary and strategically viable.
% DISAPPEARANCE_RATIONALE: Global food, research, and resource systems depend on treating animals as property. Abolition would require rapid transformation of agriculture (plant-based, cellular, synthetic systems), research (computational, in vitro, tissue models), and consumption patterns. Supply chains would collapse and reorganize. Economies built on animal extraction would shrink or restructure. A world without animal property status would be unrecognizable.
% FOUNDING_PROBLEM: Utility extraction: how to control animal bodies and their outputs as commodities for human benefit. Property law solved this by treating animals as things owned rather than beings with interests.
% FOUNDING_PROBLEM_CORROBORATION: Historians and animal studies scholars document that property status was a deliberate legal-conceptual choice, not natural or inevitable. The founding problem (how to extract utility) has been thoroughly solved — we have industrialized animal extraction. The abolitionist contest is whether the answer (property status) should stand, not whether the problem itself is still live. Industries and legal establishments attest the problem persists (animals remain economically useful); advocates and scholars attest the problem has been *answered* and the question is now about the justice of the answer. No corroboration exists from animals themselves.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-ceiling (0.92) because the abolitionist reading defines ANY use of animals as property as rights violation — there is no welfare threshold above which use becomes acceptable. The metric is stable across the interval because the fundamental injustice (animals as property) does not change; regulatory reforms may temporarily reduce some animals' suffering but do not address the root claim. Suppression is high (0.78–0.80) because the regime's persistence depends on active legal enforcement (criminal law against animal liberation, property law protecting owners' discretion, agricultural regulations that presume property status). The suppression measurement tracks enforcement-capacity maintenance: as abolitionist advocacy grows, enforcement machinery may need intensification to preserve property status (heightened penalties, criminalized advocacy, tighter borders on sanctuary). Theater is low (0.22–0.25) because abolitionism cannot hide behind welfare-improvement narratives — it explicitly names the entire regime as unjust. Accessibility collapse is high (0.88) because animals have no structural exit: they cannot refuse ownership, negotiate terms, or leave the property regime. For humans embedded in the system (consumers, workers), exit is constrained (dietary change, career shift, relocation to abolitionist communities) but not impossible. The constraint's accessibility closure operates at the level of the animals themselves (complete), not the human beneficiaries (partial). Resistance is high (0.72) because abolitionism is an organized, growing movement making explicit rights claims; welfare reform is institutionally integrated (lower resistance). The core tension with the welfare reading is empirical: does incremental regulation extend the property regime's life and delay abolition (abolitionist claim), or does it create political and economic momentum for further change (reformer claim)?
 *
 * PERSPECTIVAL GAP:
 *   The victim seat (animals) computes as trapped, powerless payer under a snare. The agenda-setter seats (industries, legal framework) compute as powerful beneficiaries coordinating economic activity. The abolitionist advocate seat computes as organized moderate-power payer bearing resistance costs. The consumer public computes as organized payer with constrained but non-zero exit. The engine produces a seat-level classification for each; the divergence between institutional-beneficiary readings and victim/advocate readings is the core measurement. The welfare reformer seat is excluded from the stakeholder list because this reading does not comprise them — they hold a different reading of the same kernel and would have a different situation statement and stake in the constraint. Authoring their perspective here would collapse the distinction between different readings; instead, they appear as excluded stakeholders whose position would contest this reading's characterization.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the clearest victim population: d approaches 1.0 (full target) because they are powerless, trapped, and bear the extraction of their entire bodies and lives. Their exit_options are structurally impossible — they cannot negotiate, refuse, or leave the property regime without external intervention at civilization scale. Industries and legal frameworks are beneficiaries: d approaches 0.0 because they collect rents from the property regime and have high exit_options (can shift business models, change legal doctrine — it is costly but possible). Abolitionist advocates are partial targets: they bear costs of advocacy (career risk, resources) but have mobile exit (can stop advocating, shift causes), so d is moderate-to-high (0.4–0.6) — they pay to challenge the constraint but are not trapped like the animals. Welfare reformers are excluded, not seated. Consumer publics are complex: they collect cheap products and research outcomes (beneficiary direction) but pay via participation in a morally unjust system and constrained exit (payer direction), so d sits near symmetric (0.5) with slight skew toward target because the constraint's persistence requires their (reluctant or unaware) compliance. No directionality overrides are warranted; the structural data (power, exit, beneficiary/victim roles) drives the derivation cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows signs of mandatrophy-in-reverse: the founding problem (utility maximization from animal bodies) remains descriptively live (industries continue extraction), but the mandate itself — the justification for property status — is increasingly contested and morally exhausted within domains that take animal sentience seriously. The regime persists through legal and market inertia, institutional capture, and consumer participation, not because the justification is widely accepted. Under the abolitionist reading, this is exactly mandatrophy: the arrangement was built for a purpose (organizing animal use for human benefit); that purpose is morally rejected by a growing constituency; but the arrangement persists because dismantling it is costly and the costs are diffuse (spread across industries, consumers, legal institutions). The measurement series (extractiveness stable, suppression gradually rising, theater gently increasing) is consistent with a constraint experiencing pressure for change but sustained by institutional inertia — exhibiting mandatrophy dynamics without yet crossing into full piton status (theater is still low, actual function is not fully evacuated).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_sentience_and_personhood,
    'Do non-human animals possess moral personhood and inherent rights, or is moral status derived from human-like rationality, social contracts, or instrumental utility?',
    'Philosophical argumentation grounded in empirical evidence of animal cognition, emotion, and social complexity; comparative legal theory examining how personhood has been extended in human contexts (corporations, entities without rationality) and applying parallel logic to animals.',
    'If animals possess inherent moral personhood independent of human recognition, the property regime is fundamentally unjust; if moral status requires human-like capacities or social recognition, the regime may be justified or require only welfare constraints rather than abolition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(animal_sentience_and_personhood, conceptual, 'Whether animal sentience and social complexity warrant classification as moral persons with rights-against-use or whether moral status is derivative and negotiable.').

omega_variable(
    use_vs_welfare_trade_off,
    'Does incremental welfare regulation extend the social license for animal property (thus delaying abolition), or does it create momentum and infrastructure for further rights-expansion that leads toward abolition?',
    'Historical-empirical study of welfare reforms in specific sectors: tracking whether regulations were followed by further restrictions or entrenchment; studying jurisdictions with stronger welfare frameworks to assess whether abolition is more likely there; interviewing reformers and abolitionists about strategic experience.',
    'If welfare reforms entrench property status, abolitionists are correct that regulation is strategically counterproductive; if welfare reforms correlate with rights-expansion trajectories, reform and abolition may be compatible strategies on a single arc.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(use_vs_welfare_trade_off, empirical, 'Whether welfare regulation accelerates or impedes movement toward animal rights and property-status abolition.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of animal liberation movements and abolitionist advocacy primarily structural (legal barriers, resource control, police enforcement) or internalized (cultural naturalization of property, consumer unawareness of alternatives)?',
    'Post-legal-change tracking: in jurisdictions that decriminalize animal advocacy or weaken property protections for specific animals, measure whether resistance persists after legal barriers fall; ethnographic study of consumer and activist cognition; comparative analysis across cultural contexts with different naturalization patterns.',
    'If suppression is mainly structural, legal abolition could succeed with enforcement; if substantially internalized, cultural and epistemic work is required alongside legal change, and the transition trajectory is slower and more fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether the extraction-enabling suppression operates through external coercion or internalized cultural scripts.').

omega_variable(
    moral_status_boundary_contestation,
    'If animals are granted property-status abolition and rights-against-use in some jurisdictions, what prevents that status from collapsing or being revoked under economic pressure or cultural backlash?',
    'Study of how other moral status gains have been defended (abolition of human slavery, women''s rights, disability recognition) — what institutional and cultural mechanisms protect rights once granted against pressure to revoke them; pilot interventions in specific sectors or regions testing stability of abolitionist law.',
    'If abolitionist status is structurally fragile and vulnerable to reversal, the constraint may degrade back toward property-status under resource pressure; if defenses exist, abolition may be durable once achieved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_status_boundary_contestation, empirical, 'Whether animal rights-status abolition, once legally established, can withstand economic and cultural pressure to revert to property regimes.').

omega_variable(
    kernel_reading_contest_structure,
    'Which reading of the animal_status_kernel will ultimately prevail, or will the kernel remain permanently contested across different jurisdictions and constituencies?',
    'Long-term institutional tracking: monitoring legislative movements toward abolition or property-rights strengthening; tracking international legal harmonization efforts; observing whether any single reading consolidates institutional dominance or whether pluralism persists.',
    'Prevalence of the abolitionist reading affects the terminal state of the constraint: abolition would shift animals from property to persons (constraint dissolves or transforms entirely); property-reading dominance would entrench the regime; perpetual contestation would sustain the constraint''s contested status indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'The long-term institutional and epistemic outcome of the animal-status kernel contest and which reading will dominate, if any.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t5, animal_status_kernel__abolitionist_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(anim_tr_t5, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__abolitionist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t15, animal_status_kernel__abolitionist_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(anim_tr_t15, projected).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__abolitionist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(anim_tr_t20, projected).
narrative_ontology:measurement(anim_tr_t25, animal_status_kernel__abolitionist_reading, theater_ratio, 25, 0.23).
narrative_ontology:measurement_basis(anim_tr_t25, projected).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__abolitionist_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(anim_tr_t30, projected).
narrative_ontology:measurement(anim_tr_t35, animal_status_kernel__abolitionist_reading, theater_ratio, 35, 0.24).
narrative_ontology:measurement_basis(anim_tr_t35, projected).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(anim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t5, animal_status_kernel__abolitionist_reading, base_extractiveness, 5, 0.89).
narrative_ontology:measurement_basis(anim_be_t5, observed).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__abolitionist_reading, base_extractiveness, 10, 0.9).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t15, animal_status_kernel__abolitionist_reading, base_extractiveness, 15, 0.91).
narrative_ontology:measurement_basis(anim_be_t15, projected).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__abolitionist_reading, base_extractiveness, 20, 0.91).
narrative_ontology:measurement_basis(anim_be_t20, projected).
narrative_ontology:measurement(anim_be_t25, animal_status_kernel__abolitionist_reading, base_extractiveness, 25, 0.92).
narrative_ontology:measurement_basis(anim_be_t25, projected).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__abolitionist_reading, base_extractiveness, 30, 0.92).
narrative_ontology:measurement_basis(anim_be_t30, projected).
narrative_ontology:measurement(anim_be_t35, animal_status_kernel__abolitionist_reading, base_extractiveness, 35, 0.92).
narrative_ontology:measurement_basis(anim_be_t35, projected).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.92).
narrative_ontology:measurement_basis(anim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t5, animal_status_kernel__abolitionist_reading, suppression_requirement, 5, 0.76).
narrative_ontology:measurement_basis(anim_su_t5, observed).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__abolitionist_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t15, animal_status_kernel__abolitionist_reading, suppression_requirement, 15, 0.77).
narrative_ontology:measurement_basis(anim_su_t15, projected).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__abolitionist_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(anim_su_t20, projected).
narrative_ontology:measurement(anim_su_t25, animal_status_kernel__abolitionist_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(anim_su_t25, projected).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__abolitionist_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(anim_su_t30, projected).
narrative_ontology:measurement(anim_su_t35, animal_status_kernel__abolitionist_reading, suppression_requirement, 35, 0.8).
narrative_ontology:measurement_basis(anim_su_t35, projected).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement_basis(anim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__abolitionist_reading, 0.25).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested animal_status_kernel. The property_reading and welfare_reading are sibling constraints instantiating alternative readings of the same foundational commitments about what animals are and what status they hold. Extractiveness and beneficiary structures differ across readings: abolitionist_reading treats all use as rights-violation (high ε, clean victim set); property_reading treats animal extraction as legitimate economic activity (low ε, no victim set); welfare_reading treats use as acceptable under regulation (moderate ε, victim set narrowed to animals subjected to unmitigated suffering). All three readings reference the same kernel commitments but interpret them incompatibly. The abolitionist_reading forecloses the property_reading's core premise (that property status is legitimate) while coexisting with the welfare_reading as a strategic disagreement over whether regulation accelerates or impedes rights-protection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
