% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary — Restrictive Anthropocentric Reading (Born Humans with Cognitive Capacity)
 *   domain: legal philosophy/constitutional law/rights theory
 *
 * SUMMARY:
 *   This story instantiates the restrictive anthropocentric reading of the
 *   legal-personhood kernel: the standing arrangement of essentially every
 *   modern legal system, under which full rights-bearing status attaches at
 *   live birth to human organisms — with cognitive capacity qualifying the
 *   margins of the class — and is withheld from every other entity: prenatal
 *   organisms, non-human animals, ecosystems, artificial systems. Assessed
 *   from this reading's own lights, the arrangement is a low-extraction
 *   coordination device: it gives law one administrable answer to the
 *   question 'who can hold rights,' and its prenatal exclusion doubles as the
 *   doctrine that keeps state intervention in pregnancy structurally
 *   unavailable, maximizing pregnant-person autonomy. The constraint is
 *   claimed as rope from the authoring seat; the metrics are authored as the
 *   reading's honest descriptive assessment of the arrangement's actual
 *   operation, independently of the claim. The kernel family (this reading
 *   plus the developmental-potentiality and functional-capacity readings) is
 *   decomposed per the epsilon-invariance principle: each reading is a
 *   separate file with its own epsilon, victim set, and classification,
 *   linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - born_rights_bearing_humans: Primary beneficiary class (organized/constrained) — the community whose claims the arrangement makes cognizable; cannot exit the category it constitutes
 *   - pregnant_persons: Protected beneficiary seat (moderate/constrained) — the prenatal exclusion operates as their autonomy shield
 *   - constitutional_courts_and_legislatures: Agenda-setter (institutional/arbitrage) — administers the line by ordinary adjudication and alone can move it
 *   - prenatal_human_organisms: Excluded class, non-agent seat (powerless/trapped) — placed outside the conversation by the attachment clause
 *   - nonhuman_high_cognition_animals: Excluded class, non-agent seat (powerless/trapped) — placed outside by the species clause
 *   - natural_ecosystems: Excluded class, non-agent seat (powerless/trapped) — objects of regulation, never subjects of claims
 *   - artificial_systems: Excluded class, non-agent seat (powerless/trapped) — assigned the status of tools ahead of any capacity question
 *   - legal_philosophers_and_comparatists: Analytical observer (analytical/analytical) — tracks the line across jurisdictions and supplies the comparative record courts cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.22).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.38).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary — Restrictive Anthropocentric Reading (Born Humans with Cognitive Capacity)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal philosophy/constitutional law/rights theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, 'cf6b695e-116b-4b3f-b1e3-2dea5e2691ed').
narrative_ontology:cs_kernel_codification('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', distributed).
narrative_ontology:cs_authority_grounding('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', practice).
narrative_ontology:cs_interpretation_layer_present('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed').
narrative_ontology:cs_reading_relation('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', foundational, personhood_attaches_at_live_birth).
narrative_ontology:cs_axiom_status(personhood_attaches_at_live_birth, holdable).
narrative_ontology:cs_axiom_grounding('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', personhood_attaches_at_live_birth, conventional).
narrative_ontology:cs_axiom('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', foundational, standing_bounded_by_human_species_membership).
narrative_ontology:cs_axiom_status(standing_bounded_by_human_species_membership, holdable).
narrative_ontology:cs_axiom_grounding('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', standing_bounded_by_human_species_membership, deontological).
narrative_ontology:cs_axiom('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', secondary, cognitive_capacity_gates_full_standing_within_the_class).
narrative_ontology:cs_axiom_status(cognitive_capacity_gates_full_standing_within_the_class, holdable).
narrative_ontology:cs_axiom_grounding('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', cognitive_capacity_gates_full_standing_within_the_class, empirically_contingent).
narrative_ontology:cs_reference_frame('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', birth_based_anthropocentric_personhood).
narrative_ontology:cs_drift_state('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', contemporary_doctrine, gap(axiom_overriding, minor, true)).
narrative_ontology:cs_created_at('cf6b695e-116b-4b3f-b1e3-2dea5e2691ed', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_rights_bearing_humans).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, constitutional_courts_and_legislatures).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, birth_criterion_of_legal_personhood).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, prenatal_state_non_interference_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold every category of enforceable claim the legal system recognizes — contract, property, tort, political rights — because the attachment clause assigned the full package at live birth. The category is not one they joined and not one they can leave; their stake is that the line stays where it is, since any widening redefines what their own rights are measured against.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_rights_bearing_humans, beneficiary,
    organized, generational, constrained, global).

% Carry pregnancies under an arrangement that assigns no independent standing to the organism they carry: the state cannot invoke fetal interests to compel or prohibit their conduct during pregnancy, and reproductive decisions sit with them by default. What flows to them is the autonomy shield; what they bear is the ordinary medical and legal exposure of pregnancy itself. Changing jurisdiction changes which version of the line applies, not whether some line does.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, national).

% Register births as the standing event, apply capacity tests at the human margins, and dismiss claims brought on behalf of entities the birth and species clauses exclude. They can move the line — doctrine has extended limited standing to corporations and, in a few jurisdictions, to natural objects — and every such move is an exercise of the same agenda-setting power that holds the line where it is. What flows to them: administrability, docket finality, and the contest costs of a line under permanent challenge.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, constitutional_courts_and_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__restrictive_anthropocentric_reading, constitutional_courts_and_legislatures, beneficiary).

% Human organisms before live birth. Under this arrangement they hold no enforceable claims in their own right; their interests reach the legal system only through other parties — the pregnant person, the state acting in criminal law, appointed guardians. They cannot appear as parties, object, or exit the category; the attachment clause places them outside it as a class.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, prenatal_human_organisms, excluded,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, prenatal_human_organisms).

% Great apes, cetaceans, elephants, and corvids whose cognitive profiles overlap parts of the human range. The species clause attaches no rights to them regardless of those profiles; their interests are handled one level down, through welfare statutes that regulate human conduct toward them without conceding standing to sue. From inside the category they are excluded from, the exclusion is not negotiable.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, nonhuman_high_cognition_animals, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, nonhuman_high_cognition_animals).

% Rivers, forests, and watersheds are objects of property and regulation, never subjects of claims. Where other jurisdictions have granted natural objects standing by statute or constitutional text, this arrangement treats those grants as foreign objects rather than extensions; environmental interests enter the legal system only as human-held claims.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, natural_ecosystems, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, natural_ecosystems).

% Software agents and artificial systems hold no standing: they cannot sue, own property in their own right, or bear duties as principals, and are dealt with as instruments of their operators. The capacity question — whether any of them demonstrates the cognition the criterion names — never gets reached, because the birth and species clauses close the category ahead of it.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_systems, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_systems).

% Track the line's doctrine across jurisdictions, document where it holds and where exceptions accumulate, and supply the comparative and theoretical analyses courts cite when the placement of the line is argued. They bear no costs and collect no benefits; their seat is analytical.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_philosophers_and_comparatists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies the legal system with a single administrable criterion for rights-bearing status: standing attaches at live birth to human organisms, with cognitive capacity qualifying the margins of the class. Every court, registry, and legislature can resolve who may hold claims, bear duties, sue, and be sued without relitigating the metaphysics of personhood in each case, and the docket of standing disputes stays finite.
% TRANSFER_FUNCTION: Allocates enforceable-claim capacity: the arrangement concentrates the capacity to hold rights, sue, and bear duties in born humans specifically, and correspondingly withholds it from every entity class outside the birth and species lines — prenatal organisms, non-human animals, ecosystems, artificial systems. Nothing material moves; what moves is standing itself. The prenatal exclusion doubles as a shield that keeps state power out of pregnancy.
% ABSENT_VOICES: The classes the arrangement places outside the conversation — prenatal organisms, high-cognition non-human animals, ecosystems, artificial systems — appear in legal process only through human representatives whose standing the arrangement itself limits: guardians, welfare statutes, environmental plaintiffs. Under this reading the absence is the arrangement operating as designed rather than a defect; descriptively, the seats are empty, and the only voices that could fill them belong to advocates the doctrine dismisses for lack of standing.
% DISAPPEARANCE_RATIONALE: Every standing question reopens at once: prenatal tort and inheritance claims, habeas petitions for apes and cetaceans, standing suits for rivers and forests, personhood petitions for artificial systems — plus the collapse of the settled criteria behind property, tort, criminal capacity, and birth registration. Courts would have to rebuild a criterion from case one, and the reproductive-autonomy shield would dissolve with the prenatal exclusion, since the state could invoke fetal interests the moment it lapsed.
% FOUNDING_PROBLEM: Law needs a determinate answer to 'who can hold rights and duties' — without one, every tort, contract, and criminal case reopens the question. Pre-modern law answered with status hierarchies; the modern settlement replaced them with a universal line: every born human a person, nothing else a person, capacity qualifying the human margins. The arrangement was built to make standing administrable at scale.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside this reading's beneficiary set: comparative legal scholarship and jurisprudence — including theorists who reject this reading's placement of the line — attest that some determinate criterion is indispensable and that the birth-based settlement is what made modern mass adjudication workable. The disagreement across the kernel is over where the line belongs, not whether law can do without one.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).
:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.22 is the reading's own assessment of the standing arrangement: the line withholds standing from classes this reading holds have no rights-relevant status, so the withholding deprives no one of anything they are entitled to; what remains is the honest residue — bright-line costs in the margin cases (viability disputes, end-of-life capacity determinations) and the administrability price of a line that cannot be fine-tuned. Suppression 0.38 is authored as a raw structural property, unscaled by scope or power (the engine owns any scaling of extractiveness): the arrangement is the settled default maintained by ordinary adjudication rather than a dedicated enforcement apparatus, but challenge volume has grown for five decades — fetal-personhood measures, animal habeas filings, ecosystem-standing suits, artificial-personhood proposals — and the dismissive work grows with it. Theater 0.16: the line does real work in every docket; the performative share is the recurring judicial passage disclaiming any definition of personhood while applying the line without difficulty. Accessibility_collapse 0.45: rival criteria remain fully arguable in scholarship and politics; the line forecloses non-person standing inside doctrine without collapsing the space where alternatives are pressed. Resistance 0.58: the line is contested simultaneously from the prenatal side and the species side — a high figure for a constraint whose extraction is this low. Claimed type: rope — the arrangement solves a genuine collective-action problem (determinate standing at scale) with minimal coercive overhead, its participants are net beneficiaries, and its rivals are litigated with, not suppressed. The declared coordination function is genuine identity_coordination: the arrangement maintains the membership category of the legal community against a constant stream of new candidacy claims (corporations, rivers, animals, machines), which is exactly the work that type describes. All measurement series share one six-point grid, and the final values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence that matters here is not among the declared parties — they all sit near the beneficiary end and should compute near-convergent types — but between this story and its sibling stories over the same kernel. From a seat that counts prenatal organisms, high-cognition animals, ecosystems, or artificial systems as rights-bearers-in-waiting, this same arrangement is mass standing-deprivation with high extraction; from this reading's seat it is a boundary doing its job. The engine computes per-seat types from the structural data each story authors; the cross-family divergence is the measurement the kernel decomposition exists to take, and it is taken across files, not inside this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party sits at or near the beneficiary end, so no directionality overrides are needed: born_rights_bearing_humans receive the standing allocation itself (d near 0.0 — the arrangement subsidizes them with the entire package of enforceable claims); pregnant_persons receive the autonomy shield the prenatal exclusion provides (d near 0.0); constitutional_courts_and_legislatures receive administrability and docket finality (d low, marginally above the human classes because they also absorb the contest costs the line generates). The four excluded classes are authored as non-agent seats (agent: false): under this reading they collect nothing and bear no extractable cost, and the structural data records that rather than feeding them through the derivation as if they were parties. The derivation chain from beneficiary declarations plus exit options reproduces the reading's assessment without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards two opposite mislabels. Reading the arrangement as a snare — because its operation excludes whole classes from the rights system — mistakes the withholding of a benefit from non-members for extraction from members: a line that never extended standing to fetuses takes nothing from them. Reading it as a mountain — because it appears in every legal system and presents as a natural kind — mistakes a constructed, movable line for a natural law: the same institutions that maintain it have extended limited standing to corporations and, in a few jurisdictions, to rivers, which is what a mountain cannot do. Rope is the honest claim from this seat: a real coordination function, low coercive overhead, net beneficiaries, and a line held in place by doctrine rather than necessity. The mandatrophy question resolves to live: every new entity class re-presents the standing problem, so the arrangement shows no sign of outliving its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This file instantiates one reading (restrictive_anthropocentric) of the kernel legal_personhood_boundary; what would the constraint''s victim set, epsilon, and classification become if either sibling reading were instantiated instead?',
    'Author developmental_potentiality_reading and functional_capacity_reading as separate stories over the same kernel with their own beneficiary/victim sets and claimed types, then compare computed classifications across the family.',
    'Under the potentiality reading, prenatal organisms enter the victim set and epsilon rises sharply, since prenatal exclusion becomes rights-deprivation; under the functional reading, high-cognition non-humans enter and some capacity-lacking humans exit, again raising epsilon. This file''s low epsilon is indexical to this reading''s criterion, not a topic-level fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: reading-indexed epsilon, victim sets, and types over the shared personhood kernel.').

omega_variable(
    disagreement_location_attachment_and_species,
    'Where exactly is the kernel disagreement located relative to this reading''s two axioms — at the attachment moment (live birth vs conception) or at the species restriction (human-bounded vs capacity-neutral) — and can the two be contested independently?',
    'Test the axioms separately: a framework could adopt the birth criterion while dropping the species restriction (a birth-based species-neutral hybrid), or keep the species restriction while moving attachment earlier. The sibling readings each reject one axiom while leaving the other''s status open.',
    'If only the species axiom falls, the arrangement becomes a birth-based species-neutral boundary approaching the functional reading''s extension; if only the attachment axiom falls, it becomes conception-anchored approaching the potentiality reading. This reading''s classification is stable only while both axioms hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_attachment_and_species, conceptual, 'The kernel contest decomposes into two independently contestable structural elements: attachment moment and species restriction.').

omega_variable(
    capacity_clause_margin_absorption,
    'Does the cognitive-capacity clause of this reading exclude any born humans from full standing (anencephalic infants, permanently vegetative persons), and does guardianship and limited-personhood doctrine fully absorb the exclusion?',
    'Doctrinal survey of how capacity-lacking born humans are treated across jurisdictions: guardianship coverage, limited personhood, death-determination practice at the vegetative margin.',
    'If the clause excludes born humans whom guardianship does not absorb, the reading has a residual victim set at its own margins and epsilon rises above the authored 0.22; if absorption is complete, the clause is administratively inert and the low epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_clause_margin_absorption, empirical, 'Whether the capacity qualifier has victims at the margins of the human class.').

omega_variable(
    periphery_exception_drift,
    'Do the periphery exceptions — corporate personhood, the handful of ecosystem-standing grants, incremental animal-standing gains — represent the beginning of frame erosion or bounded exceptions the interpretive layer will continue to absorb?',
    'Track whether ecosystem-personhood and animal-standing doctrines proliferate beyond the current pockets or remain isolated; watch whether any court grounds a non-person grant in the capacity clause rather than in statutory novelty.',
    'Proliferation would raise the drift_state magnitude and pressure the species axiom toward overriding; bounded absorption leaves the reference frame intact with drift remaining minor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periphery_exception_drift, empirical, 'Whether the reading''s minor axiom_overriding drift is the start of erosion or noise the interpretive layer absorbs.').

omega_variable(
    administrability_cost_accounting,
    'Is the reading''s low epsilon stable, or do the bright-line costs — viability disputes, end-of-life capacity determinations, the litigation friction of margin cases — accumulate into extraction that the reading''s assessment undercounts?',
    'Docket analysis of margin-case volume and outcomes over the interval, and comparison of the arrangement''s contested-case share against comparable legal demarcations.',
    'Accumulation would raise epsilon above 0.22 and, from seats that count margin-case parties as payers, push the computed type toward tangled_rope; stability supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrability_cost_accounting, empirical, 'Whether the low extraction assessment survives accounting for the bright line''s margin-case costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(lega_tr_t0, observed).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(lega_tr_t10, observed).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(lega_tr_t20, observed).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement_basis(lega_tr_t30, observed).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(lega_tr_t40, observed).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement_basis(lega_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(lega_be_t0, observed).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement_basis(lega_be_t10, observed).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement_basis(lega_be_t20, observed).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.2).
narrative_ontology:measurement_basis(lega_be_t30, observed).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.21).
narrative_ontology:measurement_basis(lega_be_t40, observed).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement_basis(lega_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(lega_su_t0, observed).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(lega_su_t10, observed).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement_basis(lega_su_t20, observed).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.34).
narrative_ontology:measurement_basis(lega_su_t30, observed).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement_basis(lega_su_t40, observed).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.38).
narrative_ontology:measurement_basis(lega_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_doctrine).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, corporate_personhood_extension).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, animal_welfare_law_regime).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_legal_personhood_doctrine).

% DUAL FORMULATION NOTE:
% The colloquial label 'legal personhood' covers three structurally distinct constraints over one kernel (legal_personhood_boundary). This file instantiates the restrictive_anthropocentric_reading: attachment at live birth, species-bounded, epsilon low from its own lights (~0.22) because the classes it excludes are, by its criterion, not rights-bearers to be deprived. The developmental_potentiality_reading (attachment at conception) authors high epsilon over the same standing arrangement — prenatal exclusion becomes mass rights-deprivation and prenatal organisms enter its victim set. The functional_capacity_reading (capacity regardless of species) authors high epsilon from a different direction — the species clause excludes high-cognition non-humans. The readings share a referent and diverge on epsilon by reading-indexed assessment; they are separate files linked through this network, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
