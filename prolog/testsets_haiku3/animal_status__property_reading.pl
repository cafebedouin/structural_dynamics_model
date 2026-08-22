% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animals as Legal Property Without Independent Moral Standing
 *   domain: legal/philosophical
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the animal_status
 *   kernel: animals are legal objects without independent moral standing;
 *   human ownership is unrestricted except by welfare statutes that constrain
 *   methods of treatment but not the fundamental right to use animals
 *   instrumentally. This reading is the dominant legal doctrine in most
 *   Western jurisdictions and is defended as obvious, natural, and
 *   self-evidently rational. However, it is one pole of a three-way kernel
 *   dispute: the abolitionist_reading denies that animals can be property at
 *   all (they are rights-holders); the welfare_reading acknowledges animal
 *   sentience and interests but treats those interests as outweighed by human
 *   interests under current framework. The property reading claims near-zero
 *   extractiveness because it frames the constraint as pure coordination
 *   (establishing property rights clarifies use) with no victim set—animals
 *   themselves have no standing to claim injury. This is structurally
 *   distinct from the other readings, which generate different victim sets
 *   and different ε values for the same institutional referent.
 *
 * KEY AGENTS:
 *   - Property_law_tradition: Authorizes and enforces the constraint that animals lack independent moral standing and can be owned as chattels or property
 *   - Animal_use_industries: Benefit from unrestricted instrumental use of animals (agriculture, research, entertainment, clothing); their rights are secured by the property framework
 *   - Legislators_and_courts: Apply and extend welfare statutes that constrain methods of use without challenging the property status itself
 *   - Abolitionist_advocates: Explicitly deny the constraint's core premise and argue for reclassification of animals as rights-holders; excluded from institutional adjudication of animal status
 *   - Welfare_advocates: Accept the property framework but contest its adequacy; push for stronger statutory constraints while remaining within the property paradigm
 *   - Animals: Assigned no legal standing under this reading; they appear in the story only as property (beneficiary/victim classification is inapplicable to non-legal-actors)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.12).
domain_priors:theater_ratio(animal_status__property_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animals as Legal Property Without Independent Moral Standing").
narrative_ontology:topic_domain(animal_status__property_reading, "legal/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'bda42e9b-c0ae-4ec2-b205-cb766081fef9').
narrative_ontology:cs_kernel_codification('bda42e9b-c0ae-4ec2-b205-cb766081fef9', fixed_text).
narrative_ontology:cs_authority_grounding('bda42e9b-c0ae-4ec2-b205-cb766081fef9', lineage).
narrative_ontology:cs_interpretation_layer_present('bda42e9b-c0ae-4ec2-b205-cb766081fef9').
narrative_ontology:cs_reading_relation('bda42e9b-c0ae-4ec2-b205-cb766081fef9', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_reading_relation('bda42e9b-c0ae-4ec2-b205-cb766081fef9', animal_status__welfare_reading, influences).
narrative_ontology:cs_axiom('bda42e9b-c0ae-4ec2-b205-cb766081fef9', foundational, animals_lack_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('bda42e9b-c0ae-4ec2-b205-cb766081fef9', animals_lack_independent_moral_standing, deontological).
narrative_ontology:cs_axiom('bda42e9b-c0ae-4ec2-b205-cb766081fef9', foundational, human_property_rights_supremacy_over_sentient_beings).
narrative_ontology:cs_axiom_status(human_property_rights_supremacy_over_sentient_beings, holdable).
narrative_ontology:cs_axiom_grounding('bda42e9b-c0ae-4ec2-b205-cb766081fef9', human_property_rights_supremacy_over_sentient_beings, conventional).
narrative_ontology:cs_reference_frame('bda42e9b-c0ae-4ec2-b205-cb766081fef9', property_law_supremacy_framework).
narrative_ontology:cs_drift_state('bda42e9b-c0ae-4ec2-b205-cb766081fef9', contemporary_welfare_statute_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bda42e9b-c0ae-4ec2-b205-cb766081fef9', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, property_law_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, welfare_advocates).
narrative_ontology:constraint_victim(animal_status__property_reading, welfare_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the legal doctrine that animals are chattels without independent moral standing. Enforces this doctrine through property law, contract enforcement, and resistance to statutory expansion beyond welfare constraints. Sets the standard for what animal status means in law. Collects no direct rent but derives institutional authority and cultural legitimacy from its role as authorizer of property rights.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, property_law_tradition, agenda_setter,
    institutional, civilizational, analytical, global).

% Agriculture, animal research, entertainment, and textile industries operate on the assumption that animals are instruments subject to human use. The property reading secures their access to animal bodies and eliminates need to justify use philosophically—use is simply property exercise. They benefit directly from the constraint by being able to externalize animal welfare costs while maintaining legal title. Their lobby power influences property-law interpretation and welfare-statute design.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_use_industries, beneficiary,
    institutional, generational, mobile, global).

% Operate within the property framework but contest its adequacy by pushing for welfare statutes and enforcement. They accept that animals can be owned but insist ownership comes with enforceable duties of care. They are partially captured by the property reading—their advocacy cannot challenge the fundamental axiom that animals lack standing—but they benefit from the constraint's legal clarity while bearing the cost of working within its confines. Their exit would require abandoning years of legislative and institutional investment.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__property_reading, welfare_advocates, beneficiary).

% Argue that animals are rights-holders with inherent value precluding all instrumental use. They are structurally excluded from the property-law framework—their core claim (animals have standing) contradicts the core axiom of the property reading. They cannot advocate within the property framework without accepting the premise they reject. Their epistemic authority is suppressed by the institutional dominance of property law. They would transform the entire institutional arrangement if given influence over it.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, abolitionist_advocates, excluded,
    organized, generational, trapped, global).

% Enact welfare statutes that constrain animal treatment without reclassifying animals as rights-holders. They operate within the property framework while responding to public concern about animal suffering. They manage the boundary between property supremacy and welfare constraint, absorbing pressure from both animal-use industries (who resist constraint) and welfare/abolitionist advocates (who push for stronger standards). Their role is symmetric: they clarify the property framework while extending its welfare provisions.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legislators, agenda_setter,
    powerful, biographical, mobile, national).

% Under the property reading, animals have no legal standing and thus no seat at the bargaining table. They appear in the constraint story only as the object of property rights and welfare statutes. They are assigned observer role (informational only) because the property reading explicitly denies them agency or independent interests. In the welfare and abolitionist readings, animals would be repositioned as victims or rights-holders respectively; under the property reading, they are juridically absent.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animals, observer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(animal_status__property_reading, animals).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__property_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear legal title to animals, enabling predictable ownership, exchange, and use without requiring philosophical justification for each use. Solves the problem of how to organize access to animal bodies for human purposes (food, clothing, research, entertainment) without incurring transaction costs of negotiating with every animal.
% TRANSFER_FUNCTION: Transfers authority over animal bodies from animals themselves (who have no legal standing) to human owners. The constraint moves decision-making power from animals to humans, without requiring payment or negotiation. Property rights holders extract the use value and productive capacity of animal bodies.
% ABSENT_VOICES: Abolitionist advocates who deny the core axiom (that animals lack standing) are excluded from the property-law framework and thus structurally prevented from advocating for reclassification of animal status. They would argue that the constraint should be replaced by a rights framework. Welfare advocates are partially included (they can advocate within welfare-statute channels) but structurally confined to incremental constraint rather than fundamental reclassification.
% DISAPPEARANCE_RATIONALE: If the property reading disappeared and were replaced by abolitionist doctrine, the world would radically rearrange: animal agriculture would shrink dramatically, research protocols would transform, property claims over animals would vanish. If replaced by welfare reading alone, the rearrangement would be moderate: use would continue but under stricter constraint. If the property reading were repealed without replacement, legal chaos would ensue because the entire system of property rights depends on animal classification as chattels. However, abolitionist advocates would argue the verdict should be 'world_rearranges' (we'd organize animal use ethically rather than permitting instrumentalism); property-law tradition would argue 'world_unchanged' (property law reflects natural hierarchy, elimination would be artificial and quickly reversed); welfare advocates would argue 'contested' (the rearrangement is contestable because it depends on axiom choice).
% FOUNDING_PROBLEM: How should legal systems classify and allocate use rights over animals? The property reading arose as Western legal traditions developed property law frameworks: animals were classified as things subject to ownership to enable predictable access for agriculture, research, and trade. The founding problem is genuine: without clear legal status, use rights over animals would be unclear and contested.
% FOUNDING_PROBLEM_CORROBORATION: The property-law tradition and animal-use industries attest the founding problem remains live: without property classification, access to animal bodies for legitimate human uses would be uncertain and contested. Welfare advocates attest the problem is partly solved but inadequately: property law clarifies title but fails to constrain methods of use sufficiently. Abolitionist advocates attest the founding problem is misconceived: the real problem is not how to allocate animal bodies efficiently but whether animals should be allocated at all. Empirical corroboration from outside the benefiting parties: jurisdictions that have expanded animal welfare statutory frameworks (EU, UK, increasingly US states) show that property classification persists despite constraint expansion, suggesting the legal problem the property framework solves (clear title) remains unresolved at the level welfare statutes address (method constraints).
narrative_ontology:disappearance_verdict(animal_status__property_reading, contested).
narrative_ontology:founding_problem_status(animal_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05 ending) because under this reading there is NO extraction—the constraint is pure legal coordination clarifying what animals are and who can own them. There is no victim because animals have no legal standing to be victimized. The beneficiary set consists of parties whose interests are secured by the property framework (industries, property-law tradition). Suppression is modest (0.12) not because animals resist but because the constraint does require active legal and cultural defense against rival readings that deny its axioms. Theater_ratio rises modestly (0.04 → 0.09) as the property framework increasingly incorporates welfare language and statutory constraint while preserving the core axiom: the 'performance' is the invocation of animal interests within a framework that denies them standing to press claims. Accessibility_collapse is low (0.15) because the constraint's alternatives (animal personhood, rights frameworks) are readily intellectually accessible; the constraint persists through institutional authority and economic lock-in, not through collapsed ability to imagine other framings. Resistance is high (0.72) because abolitionist and welfare advocates actively challenge the axiom, and this challenge has achieved significant cultural traction and legislative foothold in the interval.
 *
 * PERSPECTIVAL GAP:
 *   The most important divergence is between the institutional seat (property-law tradition) and the abolitionist seat. From the institutional seat, animals lack standing to claim extraction because they lack legal personhood—the constraint is coordination pure. From the abolitionist seat, that very denial of standing IS the extraction mechanism: the property framework forecloses the alternative reading (animals as rights-holders) and forces participants to choose between accepting animal instrumentalism or rejecting the property law entirely. Welfare advocates occupy an intermediate gap: they accept the property framework but contest its adequacy, experiencing the constraint as partially coordinating (clarifying use rules) and partially extractive (confining their advocacy to welfare statutes when they would prefer rights-based constraints). The measurement series capture this gap as rising theater_ratio: the framework increasingly performs concern for animal interests while the core axiom (animals lack moral standing) remains unchanged.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for human stakeholders: institutional property advocates (d ≈ 0.1, near beneficiary); animal-use industry actors (d ≈ 0.15, near beneficiary); legislators adopting welfare measures (d ≈ 0.5, symmetric—they clarify both property rights and welfare constraints); welfare advocates (d ≈ 0.7, partial target—constrained to operate within the property framework); abolitionist advocates (d ≈ 0.95, near full target—the constraint forecloses their entire reading). Directionality for animals under the property reading is undefined (they have no legal standing). This is structurally different from the abolitionist reading (where animals would be the central target with high d) and the welfare reading (where animals would be beneficiaries of constraint-enforcing welfare statutes, d ≈ 0.3).
 *
 * MANDATROPHY ANALYSIS:
 *   The property reading does NOT face mandatrophy in its pure form. The founding problem it solves (clarifying legal title to animals, enabling predictable ownership and exchange) remains live and contentious—property law must actively defend against abolitionist and welfare challenges. The constraint's core axiom (animals lack independent moral standing) is actively under challenge, but it has not outlived its function; it persists precisely because it functions to secure property interests. However, the rising theater_ratio and measurement of welfare-statute expansion suggest emerging tension: the framework is accommodating welfare language and constraint while preserving the core axiom. This is a sign not of mandatrophy but of axiom drift risk—if welfare concerns continue to accumulate, the property reading may eventually face the mandatrophy condition where it is kept in place by inertia (nobody reorganizes the entire property system) but its core axiom (animals lack standing) is so eroded by welfare constraint that it no longer justifies the system it anchors. That risk is not yet realized and is captured in the axiom_drift omega.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contested_foundational_premise,
    'Does the property reading''s core axiom—that animals lack independent moral standing—represent a discovered truth about moral reality, or an institutional convenience that benefits property-holding classes?',
    'Comparative institutional analysis: does the axiom persist in cultures/legal traditions that did NOT develop ownership-driven economies? Do foundational texts of the reading''s own tradition show the axiom as derived or foundational?',
    'If discovered truth: the reading''s classification as low-extraction rope stands. If institutional convenience: the reading itself becomes a false summit (inherited structural beneficiary class defending its access via claimed natural law), and ε should rise toward the welfare or abolitionist readings'' higher values.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contested_foundational_premise, conceptual, 'Sibling kernel reading: is the property reading''s axiom foundational or institutionally inherited?').

omega_variable(
    welfare_statute_integration,
    'Do welfare statutes constitute genuine constraints on owner behavior, or do they function primarily as liability limitations and public-relations theater?',
    'Audit of enforcement: conviction rates under animal welfare statutes relative to violation frequency; economic analysis of compliance costs vs. avoided welfare investments; comparative jurisdiction study of constraint stringency.',
    'If genuine constraint: extraction remains near-zero (ε 0.05) and the reading''s suppression value is justified (suppression is active enforcement preventing stronger standards). If theater: ε rises to 0.15–0.25 and suppression must rise to reflect the machinery devoted to appearing constrained while permitting instrumental use to continue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_integration, empirical, 'Are welfare statutes operative constraints or theater defending unrestricted use?').

omega_variable(
    animal_interest_acknowledgment_drift,
    'Is the measured rise in theater_ratio (0.04 → 0.09 midpoint) evidence that the property reading is accommodating welfare vocabulary while maintaining property supremacy, or is it evidence that the axiom itself is weakening?',
    'Discourse analysis of property-law scholarship and legislative debate over the interval: do property lawyers increasingly invoke animal interest language in defense of property rights (accommodation), or do they increasingly concede that animals have interests but assert property rights override them (axiom drift)?',
    'If accommodation: the reading persists as a stable property framework with rising cosmetic constraint language; theater_ratio is a symptom of stability. If axiom drift: the property reading is transitioning toward the welfare reading; ε should rise and the axiom status should shift from holdable to overridden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(animal_interest_acknowledgment_drift, conceptual, 'Is rising theater_ratio accommodation within property law or evidence of axiom instability?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(anim_tr_t8, animal_status__property_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(anim_tr_t16, animal_status__property_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(anim_tr_t24, animal_status__property_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(anim_tr_t32, animal_status__property_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement(anim_tr_t40, animal_status__property_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(anim_be_t8, animal_status__property_reading, base_extractiveness, 8, 0.04).
narrative_ontology:measurement(anim_be_t16, animal_status__property_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement(anim_be_t24, animal_status__property_reading, base_extractiveness, 24, 0.05).
narrative_ontology:measurement(anim_be_t32, animal_status__property_reading, base_extractiveness, 32, 0.06).
narrative_ontology:measurement(anim_be_t40, animal_status__property_reading, base_extractiveness, 40, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__property_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(anim_su_t8, animal_status__property_reading, suppression_requirement, 8, 0.1).
narrative_ontology:measurement(anim_su_t16, animal_status__property_reading, suppression_requirement, 16, 0.11).
narrative_ontology:measurement(anim_su_t24, animal_status__property_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement(anim_su_t32, animal_status__property_reading, suppression_requirement, 32, 0.13).
narrative_ontology:measurement(anim_su_t40, animal_status__property_reading, suppression_requirement, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three structurally distinct constraint stories: property_reading (animals as legal objects, ε ≈ 0.05), welfare_reading (animals as sentient interests-bearers constrained by welfare statutes, ε ≈ 0.35), and abolitionist_reading (animals as rights-holders precluded from instrumental use, ε ≈ 0.65). All three share the same institutional referent (the current legal status of animals) but author incompatible victim/beneficiary structures and axioms because they instantiate different readings of what animals fundamentally are. The property reading forecloses the abolitionist reading (the core axiom of one directly contradicts the core axiom of the other) and influences the welfare reading (by constraining it to operate within the property framework). Each story carries its own ε value, own beneficiary/victim structure, own cs_structure with reading_relations and axioms. The three together form a kernel family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
