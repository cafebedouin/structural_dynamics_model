% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animal Moral Status — Property Reading (Animals as Chattel Resources)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested animal_moral_status
 *   kernel: the property reading, under which animals are chattel resources
 *   with no independent moral standing and their interests are subordinate to
 *   human interests by definition. The standing arrangement under contest —
 *   the referent for every metric below — is the existing instrumental-use
 *   arrangement assessed by THIS reading's own lights: using one's property
 *   is not extraction, so epsilon is very low; the only discipline the
 *   reading concedes is waste/inefficiency of the owner's own asset. The
 *   claim/metric gap is deliberate and load-bearing: the reading CLAIMS
 *   mountain (bedrock natural order, self-evident, undefended) while the
 *   authored metrics record a frame that increasingly needs active defense —
 *   rising suppression infrastructure and rising performative reaffirmation
 *   across the postwar interval. The engine measures that divergence; this
 *   file does not reconcile it. KEY AGENTS (by structural relationship): -
 *   commercial_animal_enterprises: Primary beneficiary and co-administrator
 *   (institutional/arbitrage) — appropriates the arrangement's product stream
 *   at industrial scale and funds its defense - individual_animal_owners:
 *   Diffuse beneficiary (moderate/mobile) — receive unconditional decision
 *   authority - legal_property_institutions: Agenda setter
 *   (institutional/analytical) — administer the chattel classification
 *   through doctrinal continuity - animals_as_owned_resources: Material
 *   cost-bearer recorded as NON-AGENT (powerless/trapped) — the reading's
 *   standing denial excludes them from the party ledger and from
 *   directionality - animal_advocacy_movements: Excluded voice
 *   (organized/identity_locked) — contest the frame from outside a
 *   conversation their claims cannot enter - moral_philosophers: Analytical
 *   observer (analytical/analytical) — map the standing question across
 *   readings CONSTRAINT FAMILY NOTE (epsilon decomposition): the colloquial
 *   label 'where animals stand' decomposes into three structurally distinct
 *   constraints sharing one referent. This property reading authors epsilon
 *   ~0.07 because it recognizes no bearer from whom extraction could flow.
 *   The welfare sibling authors moderate epsilon over the same referent
 *   (suffering is cognizable, use remains permissible). The abolitionist
 *   sibling authors near-maximal epsilon over the same referent (property
 *   status itself is the violation; every use perpetuates it). Same referent,
 *   reading-indexed values — the divergence IS the data. Each reading is a
 *   separate file linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.07).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.42).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Moral Status — Property Reading (Animals as Chattel Resources)").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '5464d083-f536-487b-b7da-80f3453682ba').
narrative_ontology:cs_kernel_codification('5464d083-f536-487b-b7da-80f3453682ba', formalized).
narrative_ontology:cs_authority_grounding('5464d083-f536-487b-b7da-80f3453682ba', lineage).
narrative_ontology:cs_interpretation_layer_present('5464d083-f536-487b-b7da-80f3453682ba').
narrative_ontology:cs_reading_relation('5464d083-f536-487b-b7da-80f3453682ba', animal_moral_status__welfare_reading, influences).
narrative_ontology:cs_reading_relation('5464d083-f536-487b-b7da-80f3453682ba', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('5464d083-f536-487b-b7da-80f3453682ba', foundational, no_independent_animal_moral_standing).
narrative_ontology:cs_axiom_status(no_independent_animal_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('5464d083-f536-487b-b7da-80f3453682ba', no_independent_animal_moral_standing, deontological).
narrative_ontology:cs_axiom('5464d083-f536-487b-b7da-80f3453682ba', secondary, human_interests_lexically_prior).
narrative_ontology:cs_axiom_status(human_interests_lexically_prior, holdable).
narrative_ontology:cs_axiom_grounding('5464d083-f536-487b-b7da-80f3453682ba', human_interests_lexically_prior, deontological).
narrative_ontology:cs_reference_frame('5464d083-f536-487b-b7da-80f3453682ba', chattel_property_natural_order).
narrative_ontology:cs_drift_state('5464d083-f536-487b-b7da-80f3453682ba', contemporary_post_sentience_amendments, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5464d083-f536-487b-b7da-80f3453682ba', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, commercial_animal_enterprises).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, individual_animal_owners).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, human_dominion_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, legal_chattel_classification).
narrative_ontology:constraint_vindicates(animal_moral_status__property_reading, anthropocentric_interest_subordination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate livestock agriculture, biomedical research, and entertainment businesses built on owned animal stock. Appropriate the full product of animals' bodies and labor — meat, dairy, eggs, data, performance — at industrial scale. Fund legislative defense of the classification, circulate model bills restricting documentation of standard practices, and litigate against challenges to owner prerogative. Can restructure or relocate operations across jurisdictions at will; the classification travels with them.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, commercial_animal_enterprises, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__property_reading, commercial_animal_enterprises, agenda_setter).

% Own companion animals and small holdings. Receive uncomplicated, uncontested decision authority over the animals in their care — breeding, confinement, sale, euthanasia — without owing justification to any third party. Can cease owning animals at any time; exit is trivial and carries no penalty.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, individual_animal_owners, beneficiary,
    moderate, biographical, mobile, national).

% Courts, legislatures, and registry systems that classify animals as chattel personal property, decline standing to animal interests, and administer the ownership boundary — transfers, liens, insurance, damages valued at market price. Adjudicate the classification through doctrinal continuity inherited from Roman and common law; treat the property status as settled background rather than a live question.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_property_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Are bred, confined, worked, transported, and killed at owner discretion. Every material cost of the arrangement — the labor extracted, the lifespans shortened, the environments controlled — falls on them. No avenue of refusal, representation, or exit exists anywhere in the structure. This story instantiates a reading that assigns their position no normative weight, so they are recorded here as a non-agent entity: present in the material structure, absent from the ledger of recognized parties.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animals_as_owned_resources, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(animal_moral_status__property_reading, animals_as_owned_resources).

% Organized campaigns and philosophical schools that contest the classification and seek standing for animal interests. Hold no seat in the arrangement's administration; their moral claims are void by the frame's own definitions, so participation in the conversation that maintains the classification is structurally unavailable to them. Leaving the contest would mean abandoning commitments fused with their members' identities and life projects.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_advocacy_movements, excluded,
    organized, generational, identity_locked, global).

% Academic ethicists and legal theorists who map the standing question across its competing answers — contractualist denials of animal standing, sentience-based welfarism, rights-based abolitionism. Take no material position in the arrangement; produce the analyses each faction cites.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, moral_philosophers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__property_reading, commercial_animal_enterprises).
narrative_ontology:fixing_cost_class(animal_moral_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, uniformly and finally, who may decide the disposition of animals: the classification converts living beings into ownable, transferable, insurable, collateralizable assets, so that every human dealing with an animal becomes a species of property dealing requiring no per-use justification. It removes animal interests from the ledger of considerations in law, commerce, and policy, giving owners exclusive decision authority.
% TRANSFER_FUNCTION: Moves the entire product of animals' bodies and labor — food, fiber, work, research data, companionship, spectacle — from animals to their owners and users, and moves decision-authority over animal lives exclusively to owners. Simultaneously moves moral and legal consideration away from animals: claims made on their behalf receive zero weight inside the frame.
% ABSENT_VOICES: Animal advocacy movements and rights-based philosophies hold no seat in the arrangement's administration; animals themselves are definitionally voiceless within it. The frame's apparent unanimity arises partly because the class most materially affected was never admissible to the conversation that produced it.
% DISAPPEARANCE_RATIONALE: If the property classification vanished overnight, ownership, sale, insurance, veterinary commerce, credit secured on livestock, food-system logistics, and research procurement would all lose their legal substrate simultaneously — the arrangement is load-bearing for the entire human-animal economy, and every named seat's position depends on it.
% FOUNDING_PROBLEM: To fix, once and for all, who controls animals and on what terms — ending recurring disputes over the disposition of valuable living creatures by converting them into ownable assets, so that human use could proceed as ordinary property dealing without per-use moral accounting.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship outside the beneficiary set — Roman-law and common-law historiography, comparative-property ethnography — attests both the founding problem (recurring pre-modern disputes over livestock and working-animal control) and the classification's continuing performance of the control-settling function in contemporary statutes and case law. Advocacy scholars corroborate the genealogy while disputing its legitimacy; no source inside or outside the beneficiary set attests that the control-settling function has lapsed.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.07 (series flat at 0.05-0.07) because, by this reading's own lights, deploying owned resources extracts from no recognized party; the residual covers the waste/inefficiency discipline owners owe their own assets and the marginal friction the frame now carries. Suppression (0.42 scalar) is authored as a RAW STRUCTURAL PROPERTY — unscaled by power or scope; only extractiveness is scaled downstream — and it is the interval's real story: the frame ran on default assumption for centuries (suppression_requirement 0.10 at T=0) and now maintains dedicated defense infrastructure (documentarian-prosecution statutes, preemption laws stripping local jurisdiction, funded ballot-initiative opposition), reaching 0.42 at T=80. Theater rises 0.08 to 0.28 as the frame shifts from invisible default to performed naturalness — dominion rhetoric, welfare gestures that leave property status intact, 'they're just animals' reaffirmations. Accessibility collapse is high (0.88): inside the frame, the alternative ontology is a category error, not an option. Resistance (0.30) is real but has never converted into institutional traction against the classification itself. All three series run on one shared nine-point grid; every metric is authored at every examined time point. Coalition-power analysis is not applicable: this reading declares no victim set, so there are no powerless victims whose coalition potential needs weighing — that analysis belongs to the sibling files.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently, but the divergence has an unusual shape here. Because the reading denies standing to the maximal cost-bearer, the engine's party ledger contains only human seats: two beneficiary seats (d near the subsidized end), an administrative seat, an excluded advocacy seat, and an observer. From the enterprise seat the arrangement is simply the normal condition of doing business; from the advocacy seat the same arrangement is an intolerable closure whose wrongness cannot even be voiced inside it. The deepest perspectival fact — that the arrangement's largest cost-bearer is structurally absent from every seat — is visible only by comparing this file to its siblings, where animals enter the ledger as agents. Advocate identity-lock is noted: exit from the contest would shatter commitments fused with members' identities, which is why the excluded seat persists despite zero institutional yield.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: commercial_animal_enterprises and individual_animal_owners sit near the subsidized end (the classification secures their use rights at no recognized cost); legal_property_institutions sit near symmetric (they administer without collecting the product stream). Animals are authored agent:false, so they feed no directionality and no chi — this is the precise structural implementation of the reading's core axiom, and it is what separates this file from its siblings at the derivation level, not merely at the metric level. The advocacy seat is excluded rather than targeted: the frame does not extract from its opponents, it renders their claims inadmissible. No directionality overrides are used — the beneficiary declarations plus exit options already produce the correct relationships, and the non-agent gate handles the one case the derivation cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (settling control of animals) is live and continuously performed, so no mandatrophy is declared: live status paired with a world_rearranges verdict produces no zombie flag. The classification's risk is not atrophy but the opposite — a v_low epsilon authored by a standing-denying reading can masquerade as certified benignity if read in isolation. The corpus guard is comparative: this file's low epsilon is premise-relative (see victim_set_self_sealing omega), and only the sibling files sharing the referent reveal what the premise excludes. The rising theater and suppression series are early drift signals worth monitoring: a frame that once ran on self-evidence and now runs on defense infrastructure is traversing the mountain-toward-maintenance gradient, and the false-summit signature (mountain claim + declared beneficiaries) is expected to fire on this story — that firing is the intended measurement, not an error to be tuned away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_construction,
    'Is animals'' property status a natural fact — a fixed feature of the order of beings — or a constructed legal-normative classification that benefits identifiable users?',
    'Cross-cultural and legal-historical comparison: human-animal biology is constant while property status varies across legal traditions and eras (sacred animals, forest-dwelling protections, sentient-being constitutional amendments); variation tracking institutional choice rather than natural kind would establish construction.',
    'If constructed, the mountain claim fails and the constraint recomputes as a maintained classification serving declared beneficiaries (the false-summit chain engages); if natural, the mountain certifies and the low epsilon is arrangement-true rather than premise-relative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_construction, empirical, 'Whether the constraint is natural law or a constructed regime presenting as natural law.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the animal_moral_status kernel — what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Institutional adoption analysis: the welfare reading moves animals into a limited victim set (cruelty becomes cognizable harm; use stays permissible); the abolitionist reading makes property status itself the violation and all use extraction. The disagreement is located entirely in the standing premise and the victim-set boundary it draws — not in any empirical question the readings share.',
    'This file''s epsilon of 0.07 exists only under the property premise; under an abolitionist instantiation of the same referent, epsilon approaches maximum with animals as universal victims. Cross-reading comparison, not within-file evidence, is the only resolution path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: kernel membership, sibling deltas, and the location of the standing dispute.').

omega_variable(
    victim_set_self_sealing,
    'Is the empty victim set a fact about the arrangement, or an artifact of the reading''s own premise — a frame that denies standing, then finds no victims because none are recognized?',
    'Cost-incidence audit decoupled from the standing premise: enumerate who physically bears confinement, slaughter, and experimental use under the arrangement, independent of whether the frame counts those bearers as moral subjects.',
    'If all material costs route to entities the reading refuses to count, the low epsilon is premise-relative rather than arrangement-relative, and the reading functions as an extraction-denial device — reclassifying this story''s evidentiary weight in any cross-kernel synthesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_self_sealing, conceptual, 'Whether the v_low epsilon reflects the arrangement or the measuring frame''s self-sealing structure.').

omega_variable(
    welfare_overlay_scope_boundary,
    'Do contemporary anti-cruelty statutes belong inside this story''s referent (making the standing arrangement a property-plus-welfare hybrid) or outside it (belonging to the welfare sibling''s file)?',
    'Scope-fixing by referent definition: this story instantiates the bare chattel classification — the rule that animals are ownable resources with subordinate interests — and treats welfare statutes as a separate constraint operating atop it, authored in the welfare sibling''s file.',
    'Mis-scoping would contaminate epsilon with welfare-reading values (partial victim recognition) and blur the family''s epsilon decomposition; correct scoping keeps each reading''s value clean over the shared referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_overlay_scope_boundary, conceptual, 'Referent boundary between the bare property classification and the welfare overlay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(amstat_property_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(amstat_property_tr_t0, observed).
narrative_ontology:measurement(amstat_property_tr_t10, animal_moral_status__property_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(amstat_property_tr_t10, observed).
narrative_ontology:measurement(amstat_property_tr_t20, animal_moral_status__property_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(amstat_property_tr_t20, observed).
narrative_ontology:measurement(amstat_property_tr_t30, animal_moral_status__property_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement_basis(amstat_property_tr_t30, observed).
narrative_ontology:measurement(amstat_property_tr_t40, animal_moral_status__property_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(amstat_property_tr_t40, observed).
narrative_ontology:measurement(amstat_property_tr_t50, animal_moral_status__property_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(amstat_property_tr_t50, observed).
narrative_ontology:measurement(amstat_property_tr_t60, animal_moral_status__property_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement_basis(amstat_property_tr_t60, observed).
narrative_ontology:measurement(amstat_property_tr_t70, animal_moral_status__property_reading, theater_ratio, 70, 0.26).
narrative_ontology:measurement_basis(amstat_property_tr_t70, observed).
narrative_ontology:measurement(amstat_property_tr_t80, animal_moral_status__property_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(amstat_property_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(amstat_property_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(amstat_property_be_t0, observed).
narrative_ontology:measurement(amstat_property_be_t10, animal_moral_status__property_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement_basis(amstat_property_be_t10, observed).
narrative_ontology:measurement(amstat_property_be_t20, animal_moral_status__property_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement_basis(amstat_property_be_t20, observed).
narrative_ontology:measurement(amstat_property_be_t30, animal_moral_status__property_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement_basis(amstat_property_be_t30, observed).
narrative_ontology:measurement(amstat_property_be_t40, animal_moral_status__property_reading, base_extractiveness, 40, 0.06).
narrative_ontology:measurement_basis(amstat_property_be_t40, observed).
narrative_ontology:measurement(amstat_property_be_t50, animal_moral_status__property_reading, base_extractiveness, 50, 0.06).
narrative_ontology:measurement_basis(amstat_property_be_t50, observed).
narrative_ontology:measurement(amstat_property_be_t60, animal_moral_status__property_reading, base_extractiveness, 60, 0.07).
narrative_ontology:measurement_basis(amstat_property_be_t60, observed).
narrative_ontology:measurement(amstat_property_be_t70, animal_moral_status__property_reading, base_extractiveness, 70, 0.07).
narrative_ontology:measurement_basis(amstat_property_be_t70, observed).
narrative_ontology:measurement(amstat_property_be_t80, animal_moral_status__property_reading, base_extractiveness, 80, 0.07).
narrative_ontology:measurement_basis(amstat_property_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(amstat_property_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(amstat_property_su_t0, observed).
narrative_ontology:measurement(amstat_property_su_t10, animal_moral_status__property_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement_basis(amstat_property_su_t10, observed).
narrative_ontology:measurement(amstat_property_su_t20, animal_moral_status__property_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(amstat_property_su_t20, observed).
narrative_ontology:measurement(amstat_property_su_t30, animal_moral_status__property_reading, suppression_requirement, 30, 0.19).
narrative_ontology:measurement_basis(amstat_property_su_t30, observed).
narrative_ontology:measurement(amstat_property_su_t40, animal_moral_status__property_reading, suppression_requirement, 40, 0.24).
narrative_ontology:measurement_basis(amstat_property_su_t40, observed).
narrative_ontology:measurement(amstat_property_su_t50, animal_moral_status__property_reading, suppression_requirement, 50, 0.29).
narrative_ontology:measurement_basis(amstat_property_su_t50, observed).
narrative_ontology:measurement(amstat_property_su_t60, animal_moral_status__property_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement_basis(amstat_property_su_t60, observed).
narrative_ontology:measurement(amstat_property_su_t70, animal_moral_status__property_reading, suppression_requirement, 70, 0.38).
narrative_ontology:measurement_basis(amstat_property_su_t70, observed).
narrative_ontology:measurement(amstat_property_su_t80, animal_moral_status__property_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement_basis(amstat_property_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__welfare_reading).
narrative_ontology:affects_constraint(animal_moral_status__property_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'animal moral status' decomposes into three readings of one kernel sharing a single referent (the standing instrumental-use arrangement). This property reading is the upstream member: its classification is the legal substrate on which welfare regulation operates and against which abolitionism defines itself. Epsilon differs by reading-indexed lights over the identical referent — ~0.07 here (no recognized extraction bearer), moderate in the welfare file (suffering cognizable within permitted use), near-maximal in the abolitionist file (property status itself is the violation). Family links are declared in all three files' affects_constraints; no member is orphaned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
