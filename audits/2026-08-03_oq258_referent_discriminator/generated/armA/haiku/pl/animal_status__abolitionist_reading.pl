% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Animal Instrumental Use as Systemic Rights Violation (Abolitionist Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   Animals are rights-holders with inherent value that precludes all
 *   instrumental use: this is the abolitionist reading of the animal_status
 *   kernel. Under this reading, the current institutional and legal framework
 *   that treats animals as property and resources for human use is
 *   fundamentally illegitimate—it violates the rights of the animals involved
 *   and is sustained only through active suppression of the abolitionist
 *   claim. The reading assesses the standing arrangement (current animal
 *   instrumentalism) as pure extraction from the perspective of the animals,
 *   who are held as captive, powerless victims. The extractiveness is zero
 *   not because the arrangement provides benefit to animals (it does not) but
 *   because, from this reading's frame, animals are RIGHTS-BEARERS whose
 *   interests are never traded off against human benefits—the constraint is
 *   not a negotiated trade but a systematic violation. This reading
 *   forecloses the welfare reading's core premise (that instrumental use with
 *   'better conditions' is permissible) and coexists with the property
 *   reading as a fundamental competing ontology.
 *
 * KEY AGENTS:
 *   - non_human_animals: powerless victims, fully in harm's way, unable to negotiate or exit
 *   - abolitionist_advocates: organized but suppressed speakers, identity-locked commitment to abolition
 *   - animal_agriculture_industry: institutional agenda-setter, controls regulatory and narrative infrastructure
 *   - consumers_of_animal_products: beneficiaries of current arrangement, largely unconscious of abolitionist claim
 *   - medical/research institutions: agenda-setter and beneficiary, structurally dependent on animal models
 *   - property_law_regimes: institutional enforcers of non-rights status
 *   - welfare_advocates: structurally excluded from core dispute because they accept instrumental-use framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.0).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.92).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Instrumental Use as Systemic Rights Violation (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, 'e8493d75-3b22-4bc1-964c-4c1e4862d291').
narrative_ontology:cs_kernel_codification('e8493d75-3b22-4bc1-964c-4c1e4862d291', distributed).
narrative_ontology:cs_authority_grounding('e8493d75-3b22-4bc1-964c-4c1e4862d291', distributed).
narrative_ontology:cs_reading_relation('e8493d75-3b22-4bc1-964c-4c1e4862d291', animal_status__welfare_reading, forecloses).
narrative_ontology:cs_reading_relation('e8493d75-3b22-4bc1-964c-4c1e4862d291', animal_status__property_reading, forecloses).
narrative_ontology:cs_axiom('e8493d75-3b22-4bc1-964c-4c1e4862d291', foundational, animals_are_rights_holders).
narrative_ontology:cs_axiom_status(animals_are_rights_holders, holdable).
narrative_ontology:cs_axiom_grounding('e8493d75-3b22-4bc1-964c-4c1e4862d291', animals_are_rights_holders, deontological).
narrative_ontology:cs_axiom('e8493d75-3b22-4bc1-964c-4c1e4862d291', foundational, instrumental_use_intrinsically_impermissible).
narrative_ontology:cs_axiom_status(instrumental_use_intrinsically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('e8493d75-3b22-4bc1-964c-4c1e4862d291', instrumental_use_intrinsically_impermissible, deontological).
narrative_ontology:cs_reference_frame('e8493d75-3b22-4bc1-964c-4c1e4862d291', instrumental_animal_use_framework).
narrative_ontology:cs_drift_state('e8493d75-3b22-4bc1-964c-4c1e4862d291', contemporary_abolitionist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e8493d75-3b22-4bc1-964c-4c1e4862d291', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, non_human_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, abolitionist_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, consumers_of_animal_products).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, scientific_and_medical_institutions).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, sentience_grounds_moral_status).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, instrumental_use_incompatible_with_rights).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, species_boundary_morally_arbitrary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct costs of instrumentalization: confinement, breeding selection, slaughter, experimentation, labor extraction. Under this reading they are moral and legal rights-holders whose interests are systematically overridden. They have no capacity to negotiate, exit, or seek remedy within the existing institutional framework. Their inclusion in the constraint occurs through the reading's assignment of them as rights-bearers whose status is being violated.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, non_human_animals, payer,
    powerless, biographical, trapped, global).

% Mobilize resources to articulate and enforce recognition of animal rights and cessation of instrumental use. They bear the costs of maintaining advocacy infrastructure, facing social/economic retaliation, and operating within institutional systems that deny their core premise. Their exit options are constrained: suppression operates through ridicule, defunding, legal barriers to messaging, and institutional marginalization.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, abolitionist_advocates, payer,
    organized, biographical, constrained, global).

% Sets and enforces the operational premise that animals are instrumental inputs to human economic value extraction. Controls legislation, subsidy flows, institutional narrative (breeding as optimization, slaughter as processing, confinement as efficiency). Defines what is legible as 'animal welfare' versus what is illegible as 'animal rights.' Maintains the enforcement infrastructure that suppresses abolitionist readings.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive the direct material benefits of animal instrumentalization: low-cost food, medical testing results, consumer goods. From the abolitionist reading, they are complicit beneficiaries of systematic rights violation, though their individual choice sets are constrained by institutional subsidy and marketing structures that make instrumental use the path of least resistance.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, consumers_of_animal_products, beneficiary,
    powerful, biographical, mobile, global).

% Use animals for toxicology, pharmacology, and safety testing as an established epistemic practice. They frame this as necessary for human safety and advancement. Their exit options are institutionally constrained: alternatives (in vitro, computational modeling) exist but carry regulatory and credentialing friction; funding flows to the established animal-model path. They are both enforcers of the constraint and structurally dependent on it.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, scientific_and_medical_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, scientific_and_medical_institutions, beneficiary).

% Codify and enforce the legal status of animals as property/resources rather than rights-bearers. This reading contests that status as fundamentally illegitimate. Legal systems maintain this status through property law, agricultural regulation, and the classification of animal harm as property damage rather than rights violation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, property_law_regimes, agenda_setter,
    institutional, generational, constrained, national).

% Seek to reduce suffering within the framework of continued use, via better conditions, slaughter methods, and regulatory standards. From the abolitionist reading, they are structurally excluded from the core dispute because their framework accepts the legitimacy of instrumental use—they are inside the constraint's operating premise rather than challenging it. This reading rejects welfare reform as legitimation theater.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_advocates, excluded,
    organized, biographical, constrained, global).

% Multiple intellectual lineages converge on animal rights (Kantian duty, utilitarian sentience counting, Regan's subject-of-a-life framework, indigenous cosmologies placing animals as kin). These are not actors but vindicated intellectual positions this reading carries forward.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, philosophical_and_advocacy_traditions, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(animal_status__abolitionist_reading, philosophical_and_advocacy_traditions).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint establishes a shared institutional and legal framework that treats animals as instrumental resources for human benefit, enabling large-scale coordinated use without continuously negotiating each animal's status. From the abolitionist reading this is false coordination—the coordination is among human users at the expense of a third party excluded from participation.
% TRANSFER_FUNCTION: Moves animal bodies, labor, and biological functions from animals to humans (meat, dairy, eggs, fur, experimentation, entertainment, transportation). Moves the costs of confinement, selection breeding, psychological deprivation, injury, and death to animals. Moves institutional and economic rents to agricultural, pharmaceutical, and consumer-goods industries.
% ABSENT_VOICES: Animals themselves have no representation in the institutional systems that determine their legal and moral status. Their interests are never voiced as their own claims. Abolitionist advocates are marginalized and excluded from policy formation by structural suppression. Welfare advocates are heard but their acceptance of use-as-such constrains the scope of the conversation. Indigenous and non-Western philosophical traditions that grant animals non-instrumental standing are systematically excluded from Western legal/academic frameworks.
% DISAPPEARANCE_RATIONALE: If animal instrumentalization and the legal/institutional apparatus supporting it disappeared, the world would reorganize: food systems would shift to plant-based agriculture and cultured alternatives; medical testing would accelerate the development of alternatives; entertainment and fashion industries would lose access to live animals; human nutrition and well-being would need to adjust but evidence suggests viability. The disappearance of the constraint is the disappearance of the economic and institutional structures that depend on treating animals as instrumental inputs.
% FOUNDING_PROBLEM: How to organize human subsistence and advancement while securing access to animal bodies, labor, and biological processes as resources. The constraint emerged historically as human communities chose to treat neighboring animal species as instrumental—available for hunting, domestication, and use in all forms.
% FOUNDING_PROBLEM_CORROBORATION: The animal agriculture industry attests the founding problem is still live—humans need nutrition and livelihoods. The medical research community attests testing requirements persist. Abolitionist advocates and independent philosophers attest the founding problem is a constructed preference, not a necessity—technological and agricultural alternatives exist and are viable; the problem persists only because institutional and economic structures treat it as foundational. Indigenous traditions (external corroboration: Oren Lyons, Winona LaDuke, and others) attest the founding problem is a colonial construction—non-instrumental relationships with animals are historically and presently viable.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).
:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero in this reading because animals are assigned rights-bearing status under which NO instrumental use is permissible. The question is not 'how much extraction is happening' but 'is the use itself legitimate'—and the reading's answer is categorically no. Suppression is very high (0.92) because the abolitionist claim is actively suppressed through: legal classification of animal harm as property damage, regulatory capture of definition of 'animal welfare,' media framing of abolition as fringe/irrational, economic marginalization of plant-based alternatives, and institutional refusal to seriously engage with the rights claim. Theater ratio rises from 0.52 to 0.68 over the interval as 'welfare' reforms proliferate—cage-free certification, humane slaughter, breeding standards—while the fundamental premise of instrumental use persists unchanged. This theater masks the underlying suppression: welfare reforms legitimate the use framework even as they create appearance of responsiveness. Accessibility collapse is high (0.78) because once the abolitionist reading is understood, the alternatives (non-instrumental relationship, plant-based food systems, non-animal testing methods) become visible, but institutional barriers (subsidy, regulation, incumbent power) prevent actual access. Resistance is substantial (0.71) because abolitionist advocacy mobilizes despite suppression: protests, consumer activism, philosophical argument, policy litigation.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes this as a snare from EVERY seat because: (1) animals as primary victims are trapped and powerless; (2) abolitionist advocates bear suppression costs that outweigh any benefit; (3) the industries and consumers sit inside an enforced framework that defines animals as non-agents. There is no seat from which this reads as coordination. The property-reading stakeholders (property law regimes, industries) would argue they are coordinating efficient resource allocation—but that is a different constraint, a different reading of the same kernel. The divergence is fundamental because the readings assign different moral status to the same entity (animals). The engine measures this per-seat; the abolitionist reading has no 'beneficiary seat' that would make this structure appear coordinated—only seats that either bear costs or suppress dissent.
 *
 * DIRECTIONALITY LOGIC:
 *   From the animal's position (trapped, powerless, victim): d = 1.0 (full target). Their bodies and interests are the extractive object; they bear all costs; they have zero exit options. From the agenda-setter industry positions (institutional, powerful, beneficiary): d = 0.0 (full beneficiary—they structure the constraint and capture its benefits). From the abolitionist advocate position (organized, suppressed, payer): d = 0.9 (near-target; they bear suppression costs and constrained exit, but retain some organizational autonomy and media access). From the consumer position (powerful, mobile, beneficiary): d = 0.2 (slight target via indirect cost of complicity and moral liability, but substantial exit optionality in high-income contexts mitigates). No directionality overrides needed: the structural derivation from roles and exit options is clean.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem under this reading is 'how to maintain access to animal bodies as resources without acknowledging animals as agents with rights.' The founding problem status is contested: industries and consumers attest it is live (we need animal products); abolitionist and external advocates attest it is dead (viable alternatives exist, the problem is constructed preference). The disappearance verdict is world_rearranges (the entire food, pharmaceutical, and entertainment infrastructure would reorganize). This alignment (dead problem + world_rearranges) signals a zombie constraint: the mechanism persists despite the justifying problem being solved or spurious. This is not the same as mandatrophy, which tracks a constraint that outlived its function. Here the function (securing access to animals) is very much active; the mandate (is this legitimate?) is what has rotted away from the abolitionist reading's perspective. The reading does not claim mandatrophy for the constraint itself—it claims the constraint is fundamentally illegitimate, regardless of whether its function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_status_grounding,
    'On what basis do animals hold moral status? Sentience (capacity to suffer), relational agency, intrinsic rights, or ecosystem embeddedness? Different bases produce different scope-of-rights claims and different lists of animals included.',
    'Philosophical analysis and empirical investigation: which grounds best cohere with our settled judgments about paradigm cases (e.g., dogs, birds, octopuses, insects) and handle edge cases consistently. Comparative analysis of abolitionist frameworks (Singer''s utilitarianism vs. Regan''s deontology vs. indigenous cosmologies).',
    'A utilitarian grounding privileges sentience and includes any creature with capacity to suffer. A deontological grounding focused on inherent rights may diverge on cognitive complexity requirements. An ecocentric grounding may include entities beyond individual animals. Different groundings may produce different scope-of-protections and negotiation space with the welfare and property readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_status_grounding, conceptual, 'The philosophical ground of animal moral status under the abolitionist reading.').

omega_variable(
    instrumental_use_universality,
    'Does the abolitionist reading preclude ALL forms of animal use, or are some uses (e.g., sanctuary care, medical rescue, consensual companionship) compatible with rights-holding status? Where is the line between use and relationship?',
    'Philosophical analysis of what counts as instrumental (treating as means only) versus relational (treating as ends in themselves, with their interests considered). Case analysis of edge scenarios: using a trained dog for medical alert purposes, captive breeding for species survival, rehabilitation of injured wild animals.',
    'A strict reading that forecloses all use faces fewer practical negotiation points but more resistance from incumbent institutional actors. A reading that permits some uses creates space for negotiation but risks legitimating instrumental frameworks under the guise of ''ethical use.'' This affects both the reading''s internal coherence and its strategic positioning relative to welfare reforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_use_universality, conceptual, 'Whether the abolitionist reading permits any forms of animal use, and what counts as legitimate non-instrumental relationship.').

omega_variable(
    scale_and_practical_transition,
    'How does the abolitionist reading address the practical transition from current large-scale animal use to post-instrumental systems? Does the reading have a theory of implementation, or does it remain a normative claim without transition mechanics?',
    'Analysis of abolitionist literature for transition proposals. Assessment of technological feasibility (cultured meat, agricultural alternatives, non-animal testing methods). Comparative analysis with other radical institutional transitions (abolition of slavery, gender-property regimes).',
    'A reading with coherent transition mechanics is more credible as a policy alternative and less dismissible as utopian. A reading without transition mechanics may be stronger as a normative position but weaker as a practical contender. This affects how seriously the institutional actors (property regimes, industries) treat the reading as a threat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scale_and_practical_transition, empirical, 'Whether the abolitionist reading has or requires a worked theory of institutional transition.').

omega_variable(
    reading_coherence_vs_welfare_distinction,
    'Is the abolitionist reading internally coherent and distinctly separated from the welfare reading, or does it risk collapsing into welfare reform under political pressure? How does the reading maintain its core claim that instrumental use is intrinsically impermissible rather than merely in need of better conditions?',
    'Comparative analysis of abolitionist and welfare literature. Assessment of institutional capture risk: do policy victories that improve conditions (cage-free, slaughter methods) signal movement toward abolition or entrenchment of the use framework under softer conditions? Historical analysis of similar movements (abolition of slavery, labor reform, gender equality) for patterns of absorption and dilution.',
    'If the reading collapses into welfare-enhanced instrumentalism, it loses its distinctive claim and becomes incorporated into the property/use framework it contests. If it maintains coherence, it remains a genuine competitor to the welfare reading but may face continued marginalization. This is a core vulnerability of any radical reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coherence_vs_welfare_distinction, empirical, 'Risk of abolitionist reading being absorbed into welfare framework through institutional compromise.').

omega_variable(
    committer_frame_alternates,
    'This story instantiates the abolitionist reading of the animal_status kernel. Sibling readings (welfare_reading, property_reading) instantiate the same kernel differently. If the core normative premise shifts—e.g., from ''animals have inherent rights'' to ''animals are interests-bearers whose interests merit consideration''—does the classification remain the same or shift to a different constraint family?',
    'Test the ε-invariance principle: is extractiveness measured relative to the standing arrangement (current animal instrumentalism, assessed by the abolitionist reading''s lights: all extraction, zero under the reading? High extraction from the property reading, partial from welfare) or does it shift with the observational frame?',
    'This omega documents that the abolitionist reading''s ε=0 for animals (because any instrumental use is a rights violation) is specific to the reading''s ontology. A welfare reading would author ε somewhere in the middle (acknowledging both coordination benefits and residual harm). The readings are not different views of one constraint; they are different constraints over a shared kernel. The ε-invariance principle means each reading gets its own story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_alternates, conceptual, 'The ε-invariance grounding: this constraint is the abolitionist reading''s assessment of current animal status arrangements, not a neutral measurement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_abolitionist_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement_basis(animal_abolitionist_tr_t0, observed).
narrative_ontology:measurement(animal_abolitionist_tr_t10, animal_status__abolitionist_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement_basis(animal_abolitionist_tr_t10, observed).
narrative_ontology:measurement(animal_abolitionist_tr_t20, animal_status__abolitionist_reading, theater_ratio, 20, 0.63).
narrative_ontology:measurement_basis(animal_abolitionist_tr_t20, observed).
narrative_ontology:measurement(animal_abolitionist_tr_t30, animal_status__abolitionist_reading, theater_ratio, 30, 0.66).
narrative_ontology:measurement_basis(animal_abolitionist_tr_t30, observed).
narrative_ontology:measurement(animal_abolitionist_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.67).
narrative_ontology:measurement_basis(animal_abolitionist_tr_t40, observed).
narrative_ontology:measurement(animal_abolitionist_tr_t50, animal_status__abolitionist_reading, theater_ratio, 50, 0.68).
narrative_ontology:measurement_basis(animal_abolitionist_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(animal_abolitionist_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(animal_abolitionist_be_t0, observed).
narrative_ontology:measurement(animal_abolitionist_be_t10, animal_status__abolitionist_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement_basis(animal_abolitionist_be_t10, observed).
narrative_ontology:measurement(animal_abolitionist_be_t20, animal_status__abolitionist_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement_basis(animal_abolitionist_be_t20, observed).
narrative_ontology:measurement(animal_abolitionist_be_t30, animal_status__abolitionist_reading, base_extractiveness, 30, 0.0).
narrative_ontology:measurement_basis(animal_abolitionist_be_t30, observed).
narrative_ontology:measurement(animal_abolitionist_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.0).
narrative_ontology:measurement_basis(animal_abolitionist_be_t40, observed).
narrative_ontology:measurement(animal_abolitionist_be_t50, animal_status__abolitionist_reading, base_extractiveness, 50, 0.0).
narrative_ontology:measurement_basis(animal_abolitionist_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(animal_abolitionist_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.87).
narrative_ontology:measurement_basis(animal_abolitionist_su_t0, observed).
narrative_ontology:measurement(animal_abolitionist_su_t10, animal_status__abolitionist_reading, suppression_requirement, 10, 0.89).
narrative_ontology:measurement_basis(animal_abolitionist_su_t10, observed).
narrative_ontology:measurement(animal_abolitionist_su_t20, animal_status__abolitionist_reading, suppression_requirement, 20, 0.91).
narrative_ontology:measurement_basis(animal_abolitionist_su_t20, observed).
narrative_ontology:measurement(animal_abolitionist_su_t30, animal_status__abolitionist_reading, suppression_requirement, 30, 0.92).
narrative_ontology:measurement_basis(animal_abolitionist_su_t30, observed).
narrative_ontology:measurement(animal_abolitionist_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.92).
narrative_ontology:measurement_basis(animal_abolitionist_su_t40, observed).
narrative_ontology:measurement(animal_abolitionist_su_t50, animal_status__abolitionist_reading, suppression_requirement, 50, 0.92).
narrative_ontology:measurement_basis(animal_abolitionist_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(animal_status__abolitionist_reading, 0.0).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three constraints corresponding to three distinct readings: abolitionist (animals are rights-holders), welfare (animals are interests-bearers), and property (animals are resources). Each reading assesses the standing arrangement differently and produces different beneficiary/victim structures. The ε-invariance principle requires separate stories: the abolitionist reading sees all current animal use as pure extraction (ε=0 for rights-violating use); the welfare reading sees mixed coordination and harm (ε moderate); the property reading sees legitimate resource allocation (ε low or near-zero from industry perspective). All three readings are live in contemporary discourse; no single framework coherently holds more than one at a time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
