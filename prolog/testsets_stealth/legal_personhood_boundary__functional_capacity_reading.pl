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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood Boundary — Functional Capacity Reading (Standing Species-Based Arrangement Under Contest)
 *   domain: legal philosophy/constitutional law/rights theory
 *
 * SUMMARY:
 *   This story instantiates the functional_capacity_reading of the
 *   legal_personhood_boundary kernel. The arrangement under contest is the
 *   standing legal personhood boundary: personhood is conferred by species
 *   membership (born humans, plus chartered corporations by statute) and
 *   withheld from every non-human, so that beings with documented
 *   self-recognition, planning, and rich emotional lives are held as
 *   property. Assessed by this reading's own lights — that personhood should
 *   follow demonstrable rationality, sentience, and self-awareness regardless
 *   of species — the standing arrangement is a real coordination structure
 *   carrying heavy asymmetric extraction: the same bright line that gives law
 *   its determinate class of rights-and-duty bearers also converts an entire
 *   class of sentient beings into ownable resources and is actively enforced
 *   against challenge. Per the epsilon-referent rule, epsilon is authored for
 *   the standing species-based arrangement as this reading assesses it
 *   (high), never for the capacity-based arrangement this reading would
 *   install. The interval maps t=0..30 onto approximately 1995..2025, the
 *   period over which comparative cognition matured, animal-standing
 *   litigation began, and investigator-penalty statutes proliferated. KEY
 *   AGENTS (by structural relationship): - human_rights_bearers: Primary
 *   beneficiary (institutional/identity_locked) — holds the exclusive
 *   personhood franchise; exit would require repudiating a constitutive
 *   identity - animal_use_industries: Concentrated beneficiary
 *   (organized/arbitrage) — captures the use-value of the excluded class and
 *   finances the boundary's defense - cognitively_complex_nonhuman_animals:
 *   Primary target (powerless/trapped) — bears full denial of standing
 *   despite documented capacities - sentient_farmed_animals: Mass-scale
 *   target (powerless/trapped) — bears the arrangement's largest volumetric
 *   extraction - future_artificial_minds: Prospective target
 *   (powerless/trapped) — reserved out of any path to status -
 *   animal_law_advocates: Excluded challenger (moderate/constrained) —
 *   dismissed before merits; the capacity question is never reached -
 *   legislatures_constitutional_courts: Agenda setter
 *   (institutional/constrained) — administers and enforces the line as
 *   settled doctrine - animal_sentience_science: Analytical observer
 *   (organized/analytical) — produces the capacity evidence the criterion
 *   would consult
 *
 * KEY AGENTS:
 *   - human_rights_bearers: primary beneficiary (institutional/identity_locked) — unconditional franchise, identity-constituted
 *   - animal_use_industries: concentrated beneficiary (organized/arbitrage) — receives the extracted use-value, funds enforcement
 *   - cognitively_complex_nonhuman_animals: primary target (powerless/trapped) — apes, cetaceans, elephants, corvids held as property
 *   - sentient_farmed_animals: mass-scale target (powerless/trapped) — tens of billions, welfare-regulated but owned
 *   - future_artificial_minds: prospective target (powerless/trapped) — no application route to status
 *   - animal_law_advocates: excluded challenger (moderate/constrained) — standing-dismissed before merits
 *   - legislatures_constitutional_courts: agenda setter (institutional/constrained) — administers the line, bears none of its costs
 *   - animal_sentience_science: analytical observer (organized/analytical) — supplies the capacity evidence base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.84).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.76).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary — Functional Capacity Reading (Standing Species-Based Arrangement Under Contest)").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal philosophy/constitutional law/rights theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '898041a8-e82e-43aa-9ba8-29612c280865').
narrative_ontology:cs_kernel_codification('898041a8-e82e-43aa-9ba8-29612c280865', fixed_text).
narrative_ontology:cs_authority_grounding('898041a8-e82e-43aa-9ba8-29612c280865', lineage).
narrative_ontology:cs_interpretation_layer_present('898041a8-e82e-43aa-9ba8-29612c280865').
narrative_ontology:cs_reading_relation('898041a8-e82e-43aa-9ba8-29612c280865', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('898041a8-e82e-43aa-9ba8-29612c280865', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_axiom('898041a8-e82e-43aa-9ba8-29612c280865', foundational, cognitive_capacity_grounds_legal_personhood).
narrative_ontology:cs_axiom_status(cognitive_capacity_grounds_legal_personhood, holdable).
narrative_ontology:cs_axiom_grounding('898041a8-e82e-43aa-9ba8-29612c280865', cognitive_capacity_grounds_legal_personhood, deontological).
narrative_ontology:cs_axiom('898041a8-e82e-43aa-9ba8-29612c280865', foundational, species_lineage_confers_no_legal_privilege).
narrative_ontology:cs_axiom_status(species_lineage_confers_no_legal_privilege, holdable).
narrative_ontology:cs_axiom_grounding('898041a8-e82e-43aa-9ba8-29612c280865', species_lineage_confers_no_legal_privilege, deontological).
narrative_ontology:cs_axiom('898041a8-e82e-43aa-9ba8-29612c280865', secondary, capacity_assessment_is_judicially_administrable).
narrative_ontology:cs_axiom_status(capacity_assessment_is_judicially_administrable, holdable).
narrative_ontology:cs_axiom_grounding('898041a8-e82e-43aa-9ba8-29612c280865', capacity_assessment_is_judicially_administrable, empirically_contingent).
narrative_ontology:cs_reference_frame('898041a8-e82e-43aa-9ba8-29612c280865', capacity_tracking_personhood_line).
narrative_ontology:cs_drift_state('898041a8-e82e-43aa-9ba8-29612c280865', contemporary_animal_sentience_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('898041a8-e82e-43aa-9ba8-29612c280865', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, human_rights_bearers).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, animal_use_industries).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_nonhuman_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, sentient_farmed_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, future_artificial_minds).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, animal_property_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every born human enters life holding unconditional legal personhood: the standing to own, contract, sue, and invoke constitutional protection, secured by species membership alone and never subject to review of their actual cognitive capacities. Leaving this position would mean renouncing a status woven into their self-conception and legal identity; nothing in the arrangement pushes them to try.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_rights_bearers, beneficiary,
    institutional, generational, identity_locked, global).

% Agriculture, biomedical research, and entertainment sectors price their business models on non-human animals being legally ownable. The boundary guarantees their input stock; they finance its defense through lobbying, litigation against investigators, and model legislation. Exit is ordinary commercial arbitrage: pivot inputs, relocate production, or substitute materials.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_use_industries, beneficiary,
    organized, biographical, arbitrage, global).

% Great apes, cetaceans, elephants, and corvids with documented self-recognition, planning, and social cognition are held as property. They cannot own, contract, sue, or appear in any forum; their interests reach decision-makers only through human proxies, and every petition filed in their name has been dismissed before the question of their status was reached. No jurisdiction exists for them to move to that lacks the property regime.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, cognitively_complex_nonhuman_animals, payer,
    powerless, biographical, trapped, global).

% Tens of billions of animals experience pain, fear, and comfort while remaining legal things. Welfare statutes regulate how they are handled but presuppose an owner; their lifespans are set by production schedules, and no exit exists short of death.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_farmed_animals, payer,
    powerless, immediate, trapped, global).

% Prospective artificial systems that may someday meet any plausible threshold of rationality, sentience, or self-awareness would arrive into a legal order that allocates personhood by species membership and corporate charter, with no application route for manufactured minds. Their position is prospective: they bear nothing yet, but the arrangement reserves no path by which they ever could.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_artificial_minds, payer,
    powerless, generational, trapped, global).

% Lawyers and organizations bring habeas and standing petitions testing whether cognitively complex animals can be rights-holders. Courts dismiss on procedural grounds before reaching the capacity question, so the advocates are kept out of the conversation where the boundary is actually decided; they keep filing because each dismissal narrows the doctrinal escape routes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_law_advocates, excluded,
    moderate, generational, constrained, national).

% Legislatures define who counts as a legal person by statute and selectively widen the category to corporations by charter; constitutional courts police the line, denying animal habeas petitions and upholding statutes that penalize undercover investigation of animal facilities. They administer the boundary as settled doctrine and bear none of its costs.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legislatures_constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Comparative cognition and welfare-science researchers produce the evidence base — self-recognition, episodic memory, emotional states across species — that any capacity-based criterion would consult. Their findings circulate freely but enter law only when a human representative carries them into a proceeding that survives procedural dismissal.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_sentience_science, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, determinate, cheaply administered criterion for the classification every legal system requires: which entities hold rights, bear duties, may sue, be sued, and own. A bright-line rule settles status questions once instead of litigating each being's inner life case by case.
% TRANSFER_FUNCTION: Moves legal standing and moral consideration from every being outside the born-human class to those inside it; operationally, it converts sentient non-humans into ownable resources whose use-value flows to human institutions and industries.
% ABSENT_VOICES: The excluded themselves — the animals whose status is being allocated — cannot appear or object in any forum; their would-be representatives are dismissed on standing before the capacity question is reached, so the conversation where personhood is decided contains no voice for the class it excludes.
% DISAPPEARANCE_RATIONALE: Property law, food production, research regulation, and the entire architecture of rights and duties presuppose a fixed person-class drawn at species. Overnight removal would leave every animal titleless, every animal-use enterprise legally impossible as constituted, and every court without a criterion for who may sue — the world rearranges around a replacement criterion or collapses into adjudicative chaos.
% FOUNDING_PROBLEM: Early law needed a fixed class of rights-and-duty bearers for ownership, obligation, and procedure to function at all; the human community drew that class around itself — later narrowed to born humans and selectively widened to chartered corporations — to make status administrable and immune from case-by-case review.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship on the development of legal personality attests the administrative origin from outside any beneficiary seat; animal-rights philosophers — the arrangement's sharpest opponents — concede that law requires a determinate person-class criterion and dispute only its content, which corroborates that the founding problem remains live. No part of this genealogy rests on beneficiary self-attestation.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.84) because, on this reading's lights, the arrangement denies legal standing to beings that possess the very capacities the law elsewhere treats as the point of personhood, and converts them into resources — near-total extraction of status from the excluded class, intensified by global scope. Suppression (0.76) is predominantly structural: standing doctrine, property law, and investigator-penalty statutes close every procedural door before the capacity question is reached; a minority share is internalized (enculturated human supremacy), handled in the suppression_mechanism_composition omega. Theater_ratio (0.32) reflects the growing welfare-statute layer, which performs concern while never touching ownership or standing — functional enforcement of the boundary continues underneath. Accessibility_collapse is moderate-low (0.45): the alternative criterion is fully articulable, scientifically supported, and persistently re-filed, so understanding the arrangement does not collapse alternatives the way a natural limit would. Resistance (0.58) is substantial and rising: sustained litigation, sentience legislation, philosophical mobilization, and direct action. Claim and metrics are independent authored facts: the claimed type is tangled_rope because the structure genuinely coordinates (every legal system needs a determinate person-class) while extracting asymmetrically through the same line, requiring active enforcement to hold. The temporal series run on one shared six-point grid; suppression_requirement is tracked because enforcement capacity genuinely built out over the interval (ag-gag proliferation, SLAPP strategies, hardening standing dismissals after the 2013 habeas filings) — an enforcement ratchet, not a static picture. Extractiveness and theater rise monotonically; no cyclical dynamic is present, so a six-point grid suffices. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the trapped, powerless target seats the arrangement operates as near-total denial — global spatial scope amplifies effective extraction on beings who cannot exit any jurisdiction. From the industry seat it operates as a subsidized input guarantee with arbitrage-grade exit. From the human beneficiary seat it operates as an unconditioned good the holders did not choose and cannot easily renounce — identity-locked into benefit, an unusual configuration where lock-in protects the beneficiary side. From the agenda-setter seat it is settled administration: courts enforce the line while bearing none of its costs. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. animal_use_industries combines beneficiary role with arbitrage exit and direct receipt of the extracted use-value — nearest the beneficiary pole. human_rights_bearers are beneficiaries with identity_locked exit: locked into the subsidized position, slightly less extreme than pure arbitrage but still low d. The three victim groups combine payer role with trapped exit and powerless power — nearest the target pole, with global scope amplifying effective extraction; future_artificial_minds carry that position prospectively. animal_law_advocates (excluded) and animal_sentience_science (observer) sit near symmetric: they neither collect nor bear the transfer, though the advocates' alignment with targets is noted for seat-divergence analysis. No directionality overrides are needed — the structural derivation captures every seat. The vindicated propositions (human_exceptionalism_doctrine, animal_property_doctrine) are listed as vindicated propositions, not beneficiaries: doctrines collect no rents.
 *
 * MANDATROPHY ANALYSIS:
 *   Claiming tangled_rope guards against two mislabels. Reading the boundary as pure extraction ignores the coordination function every legal system demonstrably needs — a fixed, cheaply administered class of rights-and-duty bearers; even the arrangement's sharpest opponents concede law requires some criterion. Reading it as pure coordination ignores that the identical line renders demonstrably sentient beings ownable and is held in place by an enforcement apparatus that suppresses the capacity question procedurally. The founding problem (determinate status allocation) remains live, so no mandatrophy is declared and no sunset clause is authored: what is contested is the criterion, not the need. The piton failure mode is also distant — enforcement is energetic, not inertial, and a concentrated beneficiary visibly profits, which is the snare-side signature the tangled_rope claim accommodates rather than the atrophied-administrator profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the legal_personhood_boundary kernel — the functional_capacity_reading. Where exactly is the disagreement with the sibling readings located, and what would each sibling change structurally?',
    'Compare the three sibling files'' victim sets and epsilon over the same referent (the standing species-based arrangement as each reading assesses it). The disagreement is located on the criterion axis: species lineage (restrictive_anthropocentric_reading), developmental potential from conception (developmental_potentiality_reading), or demonstrated capacity now (this reading). Under restrictive displacement the victim set loses all non-humans and the contested edge becomes cognitively atypical born humans; under developmental-potentiality displacement the victim set adds conception-onset human trajectories while every non-human stays excluded.',
    'Victim-set membership and epsilon are reading-indexed; this file''s values are void under sibling displacement, and cross-reading aggregation of epsilon is invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of the personhood-boundary kernel, with the structural delta each sibling would produce.').

omega_variable(
    capacity_threshold_operationalization,
    'Where inside capacity space does the threshold sit — sentience alone, self-awareness, or full rationality — and what procedure measures it for legal purposes?',
    'Comparative cognition research converging on validated markers, plus a deliberative or judicial standard-setting process specifying which markers are dispositive and who bears the burden of demonstration.',
    'A sentience-only threshold sweeps in nearly all vertebrates and cephalopods and raises measured extraction sharply; a rationality-plus-self-awareness threshold narrows the victim set to a few taxa and lowers it. Threshold breadth scales the victim set and the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_threshold_operationalization, empirical, 'Operational indeterminacy of the capacity criterion the reading depends on.').

omega_variable(
    naturality_of_species_line,
    'Is the species line a natural-kind fact the law merely registers, or a constructed privilege maintained by identifiable beneficiaries?',
    'Test whether the line tracks any biologically principled discontinuity that coincides with moral-status-relevant capacities; if capacity distributions cross the species line without legal consequence, the line is constructed rather than registered.',
    'If constructed, the arrangement is a defended privilege with identifiable beneficiaries (as authored here); if natural, part of the measured extraction is misattributed and the boundary approaches a fixed limit no reform could move.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_of_species_line, conceptual, 'Natural-kind versus constructed-privilege status of the species boundary.').

omega_variable(
    sentience_statutes_absorption,
    'Do recent statutory sentience recognitions (EU Lisbon Article 13, UK Animal Welfare (Sentience) Act 2022) erode the property boundary or theatrically absorb the challenge while leaving status untouched?',
    'Track whether any sentience recognition has ever altered a property, standing, or ownership outcome; if outcomes never move, the statutes are performative maintenance and the theater_ratio trajectory is confirmed as rising.',
    'If absorptive, the arrangement is more stable than its concession surface suggests and the theater_ratio series understates drift toward performance; if erosive, the boundary is a transitional arrangement already decomposing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_statutes_absorption, empirical, 'Whether welfare-era concessions are functional erosion or theatrical stabilization.').

omega_variable(
    prospective_artificial_minds,
    'Can future artificial systems satisfy ''demonstrable'' cognitive capacity, and does this reading apply substrate-neutrally or smuggle in a biological-substrate requirement?',
    'Observe whether capacity-based arguments advanced for animals are extended consistently to candidate artificial systems meeting the same behavioral markers, or quietly restricted to biological organisms.',
    'If substrate-neutral, the victim set expands prospectively and the reading''s species-neutrality is sincere; if substrate-restricted, the reading reproduces a lineage privilege in new form and its anti-exceptionalist axiom is compromised.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prospective_artificial_minds, empirical, 'Future-artificial-mind admission tests the reading''s neutrality claim.').

omega_variable(
    suppression_mechanism_composition,
    'How much of the measured suppression is structural (standing doctrine, property law, investigator-penalty statutes) versus internalized (enculturated human supremacy that reduces internal challenge even where barriers fall)?',
    'Compare jurisdictions differing in legal barriers at similar levels of cultural enculturation, and track challenge rates after barrier removals; persistence of acquiescence after barriers fall indicates internalized residue.',
    'If suppression is overwhelmingly structural, removing the enforcement machinery releases resistance quickly and the arrangement is brittle; if a large internalized share exists, the arrangement persists after legal reform and effective suppression exceeds the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of the boundary''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(lega_tr_t0, observed).
narrative_ontology:measurement(lega_tr_t6, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 6, 0.23).
narrative_ontology:measurement_basis(lega_tr_t6, observed).
narrative_ontology:measurement(lega_tr_t12, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(lega_tr_t12, observed).
narrative_ontology:measurement(lega_tr_t18, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(lega_tr_t18, observed).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement_basis(lega_tr_t24, observed).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(lega_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(lega_be_t0, observed).
narrative_ontology:measurement(lega_be_t6, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 6, 0.74).
narrative_ontology:measurement_basis(lega_be_t6, observed).
narrative_ontology:measurement(lega_be_t12, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 12, 0.77).
narrative_ontology:measurement_basis(lega_be_t12, observed).
narrative_ontology:measurement(lega_be_t18, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 18, 0.8).
narrative_ontology:measurement_basis(lega_be_t18, observed).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement_basis(lega_be_t24, observed).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 30, 0.84).
narrative_ontology:measurement_basis(lega_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(lega_su_t0, observed).
narrative_ontology:measurement(lega_su_t6, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(lega_su_t6, observed).
narrative_ontology:measurement(lega_su_t12, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(lega_su_t12, observed).
narrative_ontology:measurement(lega_su_t18, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement_basis(lega_su_t18, observed).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement_basis(lega_su_t24, observed).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(lega_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the legal personhood boundary' conflates three structurally distinct constraints — three readings of one kernel. Each reading yields a different victim set, a different epsilon over the same standing arrangement, and a different classification; per the epsilon-invariance principle they are separate stories linked by network edges rather than one story with a criterion parameter. This (functional-capacity) file is downstream of the empirical animal-cognition literature and upstream of animal-standing litigation strategy; the restrictive sibling is the currently operative legal default, and the developmental-potentiality sibling shares this reading's interest in the boundary's foundations while assigning opposite statuses at both edges (embryos, capable animals).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
