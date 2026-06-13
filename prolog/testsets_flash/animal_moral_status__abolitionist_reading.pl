% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__abolitionist_reading, []).

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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Abolitionist Reading of Animal Moral Status: Property as Violation
 *   domain: applied_ethics/legal_philosophy/animal_studies
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal moral
 *   status, where animals are considered rights-bearing individuals and their
 *   property status is the fundamental violation. All use, however 'humane,'
 *   is seen as perpetuating victimization. The constraint is framed as a
 *   snare because it identifies a system of pure extraction (property status)
 *   that is maintained by overwhelming human power and suppression of animal
 *   interests. The metrics reflect the severity of this extraction and
 *   suppression from the perspective of the animals.
 *
 * KEY AGENTS:
 *   - abolitionist_advocates: Agenda-setter (organized/constrained) — seek to dismantle property status.
 *   - all_animals_under_human_dominion: Payer (powerless/trapped) — bear the full cost of property status.
 *   - animal_welfare_advocates: Excluded (organized/constrained) — their efforts, while reducing suffering, are seen as legitimizing property status.
 *   - animal_use_industries: Excluded (institutional/mobile) — primary beneficiaries and enforcers of the property status.
 *   - legal_philosophers: Observer (analytical/analytical) — analyze the conceptual foundations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Abolitionist Reading of Animal Moral Status: Property as Violation").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/legal_philosophy/animal_studies").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '663e7445-a826-41fe-99fc-ddc41579cc8a').
narrative_ontology:cs_kernel_codification('663e7445-a826-41fe-99fc-ddc41579cc8a', distributed).
narrative_ontology:cs_authority_grounding('663e7445-a826-41fe-99fc-ddc41579cc8a', diffuse_epistemic).
narrative_ontology:cs_reading_relation('663e7445-a826-41fe-99fc-ddc41579cc8a', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('663e7445-a826-41fe-99fc-ddc41579cc8a', animal_moral_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('663e7445-a826-41fe-99fc-ddc41579cc8a', foundational, animals_are_rights_bearing_individuals).
narrative_ontology:cs_axiom_status(animals_are_rights_bearing_individuals, holdable).
narrative_ontology:cs_axiom_grounding('663e7445-a826-41fe-99fc-ddc41579cc8a', animals_are_rights_bearing_individuals, deontological).
narrative_ontology:cs_axiom('663e7445-a826-41fe-99fc-ddc41579cc8a', foundational, property_status_is_the_violation).
narrative_ontology:cs_axiom_status(property_status_is_the_violation, holdable).
narrative_ontology:cs_axiom_grounding('663e7445-a826-41fe-99fc-ddc41579cc8a', property_status_is_the_violation, deontological).
narrative_ontology:cs_reference_frame('663e7445-a826-41fe-99fc-ddc41579cc8a', animal_personhood_ideal).
narrative_ontology:cs_drift_state('663e7445-a826-41fe-99fc-ddc41579cc8a', contemporary_legal_systems, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('663e7445-a826-41fe-99fc-ddc41579cc8a', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Argue for the full legal personhood of animals and the abolition of their property status. They seek to dismantle all systems of animal use, including those deemed 'humane,' viewing property status itself as the fundamental violation. Their efforts are primarily educational and legislative.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, generational, constrained, global).

% Are treated as property, subject to human use and ownership. Their interests are systematically subordinated to human interests, and their lives are largely determined by human decisions, regardless of welfare considerations. This constraint identifies their property status as the core violation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion, payer,
    powerless, immediate, trapped, universal).

% Focus on minimizing animal suffering within existing systems of use, advocating for better living conditions, slaughter methods, and regulations. From the abolitionist perspective, their work, while reducing suffering, implicitly legitimizes the property status of animals, thus perpetuating the fundamental violation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_welfare_advocates, excluded,
    organized, biographical, constrained, global).

% Benefit from the current legal status of animals as property, enabling their use in agriculture, research, entertainment, and other sectors. They actively resist any changes to this status that would restrict their operations or increase costs. From the abolitionist perspective, they are the primary enforcers and beneficiaries of the 'property' constraint.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_use_industries, excluded,
    institutional, generational, mobile, global).

% Analyze the conceptual and ethical foundations of animal personhood, property law, and moral rights. They evaluate the coherence and implications of different readings of animal moral status, including the abolitionist position, without direct involvement in advocacy or industry.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not primarily solve a coordination problem; rather, it identifies a fundamental moral and legal incoherence in the current human-animal relationship, aiming to coordinate human behavior around a principle of non-use and respect for animal personhood.
% TRANSFER_FUNCTION: It identifies the transfer of autonomy, bodily integrity, and life itself from animals to humans, enabled by their property status. The constraint aims to abolish this transfer.
% ABSENT_VOICES: The animals themselves are the primary absent voices, unable to articulate their interests or consent to their property status. Their interests are represented by abolitionist advocates, but their direct voice is absent from the legal and ethical discourse that defines their status.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, the entire structure of human-animal interaction would fundamentally rearrange. Industries reliant on animal use would collapse or transform, legal systems would need to redefine personhood and rights, and human ethical frameworks would undergo a profound shift. The world would be unrecognizable in its treatment of animals.
% FOUNDING_PROBLEM: The historical and ongoing problem of human exploitation and instrumentalization of animals, rooted in their legal classification as property, leading to systemic suffering and denial of inherent worth.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist advocates attest that the problem is profoundly live, citing the scale of animal agriculture, research, and other forms of use. Independent ethical philosophers and some legal scholars corroborate that the fundamental moral status of animals remains an unresolved and pressing issue, with the property paradigm being a central point of contention.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the property status of animals allows for their complete instrumentalization, denying their fundamental interests and autonomy. Suppression is also extremely high (0.98) as animals are legally and physically unable to resist their status, and human systems are designed to maintain this control. Theater ratio is very low (0.05) because, from this perspective, there is little performative 'coordination' masking the direct extraction; the system is overtly about use. Accessibility collapse is near total (0.99) as there are virtually no alternatives for animals to escape their property status. Resistance is minimal (0.02) from the animals themselves, though human advocates provide some resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animal use industries, animals are property, and their use is a natural right or economic necessity. From the abolitionist perspective, this is a system of profound injustice and extraction. The engine's classification will highlight this divergence by computing a snare from the animals' seat, contrasting sharply with the 'natural' or 'beneficial' framing by human users.
 *
 * DIRECTIONALITY LOGIC:
 *   All animals under human dominion are the full targets (d=1.0) as they bear the entire cost of their property status. Abolitionist advocates are agenda-setters, working to dismantle the constraint. Animal welfare advocates and animal use industries are 'excluded' from the core abolitionist framing, as their positions either implicitly or explicitly uphold the property status that this constraint defines as the violation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (the property status of animals) is seen as an ongoing, active violation rather than an atrophied coordination function. The analysis prevents mislabeling by focusing on the structural extraction inherent in property status itself, rather than on the 'humane' aspects of animal use, which are seen as theatrical maintenance of the underlying snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_contingency,
    'Is the property status of animals a contingent legal construct, or an inevitable outcome of human-animal interaction?',
    'Legal and philosophical arguments demonstrating the possibility and coherence of animal personhood, coupled with shifts in societal norms and legal frameworks.',
    'If contingent, the constraint is a pure snare, entirely human-made and alterable. If inevitable, it might approach a ''mountain'' of human nature or social structure, though still extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency, conceptual, 'Ambiguity regarding the fundamental nature of animal property status.').

omega_variable(
    abolitionist_vs_welfare_efficacy,
    'Does the pursuit of abolition (this reading) or welfare reform (welfare_reading) offer a more effective path to reducing animal suffering and promoting animal interests?',
    'Empirical studies on the long-term impact of different advocacy strategies on animal well-being and legal status, and philosophical debate on the ethical priority of rights vs. suffering.',
    'If welfare reform proves more effective in practice, the abolitionist reading''s strategy might be re-evaluated, though its core ethical premise would remain. If abolitionist strategies lead to significant legal shifts, it would validate this reading''s approach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(abolitionist_vs_welfare_efficacy, empirical, 'Strategic efficacy of abolition vs. welfare approaches.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the ''animal_moral_status'' kernel, or an independent claim?',
    'Analysis of the historical and philosophical lineage of abolitionist thought in relation to broader debates on animal moral status, confirming its position within the contested kernel.',
    'If not a reading, it would be reclassified as an independent constraint, losing its direct structural relationship to the ''property_reading'' and ''welfare_reading'' siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''abolitionist_reading'' of the ''animal_moral_status'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_moral_status__abolitionist_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(anim_tr_t1990, animal_moral_status__abolitionist_reading, theater_ratio, 1990, 0.02).
narrative_ontology:measurement(anim_tr_t2010, animal_moral_status__abolitionist_reading, theater_ratio, 2010, 0.04).
narrative_ontology:measurement(anim_tr_t2024, animal_moral_status__abolitionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_moral_status__abolitionist_reading, base_extractiveness, 1970, 0.98).
narrative_ontology:measurement(anim_be_t1990, animal_moral_status__abolitionist_reading, base_extractiveness, 1990, 0.97).
narrative_ontology:measurement(anim_be_t2010, animal_moral_status__abolitionist_reading, base_extractiveness, 2010, 0.96).
narrative_ontology:measurement(anim_be_t2024, animal_moral_status__abolitionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_moral_status__abolitionist_reading, suppression_requirement, 1970, 0.99).
narrative_ontology:measurement(anim_su_t1990, animal_moral_status__abolitionist_reading, suppression_requirement, 1990, 0.99).
narrative_ontology:measurement(anim_su_t2010, animal_moral_status__abolitionist_reading, suppression_requirement, 2010, 0.98).
narrative_ontology:measurement(anim_su_t2024, animal_moral_status__abolitionist_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_moral_status' kernel. The 'property_reading' asserts animals are property; the 'welfare_reading' focuses on minimizing suffering within use. This 'abolitionist_reading' asserts animals are rights-bearing individuals and property status is the violation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
