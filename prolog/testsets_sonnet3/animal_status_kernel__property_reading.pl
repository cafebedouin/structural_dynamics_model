% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Animal-as-Property Legal Status (Property Reading)
 *   domain: moral philosophy / animal ethics / legal theory
 *
 * SUMMARY:
 *   This story instantiates the property reading of the contested
 *   animal-status kernel: animals are legal property, moral considerability
 *   is derivative of ownership rights, and economic value is treated as the
 *   only relevant measure of value in use decisions. Anti-cruelty statutes
 *   exist within this reading but function to protect the owner's property
 *   interest (preventing wanton destruction of a valuable asset) rather than
 *   to recognize any interest held by the animal itself. This is a clean,
 *   ε-invariant instantiation of one reading only — the welfare_reading
 *   (property retained but welfare-constrained) and abolitionist_reading
 *   (property status itself is the injustice) are separate constraints with
 *   their own ε and stakeholder structures, linked here via network only, not
 *   folded into this story's classification.
 *
 * KEY AGENTS:
 *   - livestock_industry_owners: primary beneficiary (institutional/arbitrage) — extracts economic value from ownership with minimal constraint
 *   - owned_animals: primary target (powerless/trapped) — bears full cost of use with no legal interest recognized
 *   - animal_welfare_advocates: excluded challenger (organized/constrained) — denied standing under this reading's own logic
 *   - legislators_and_courts: agenda-setting institution (institutional/analytical) — authors and could revise the kernel's reading
 *   - consumers_of_animal_products: diffuse beneficiary (moderate/mobile) — benefits from externalized costs without bearing constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__property_reading, 0.91).
domain_priors:suppression_score(animal_status_kernel__property_reading, 0.72).
domain_priors:theater_ratio(animal_status_kernel__property_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(animal_status_kernel__property_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__property_reading, tangled_rope).
narrative_ontology:human_readable(animal_status_kernel__property_reading, "Animal-as-Property Legal Status (Property Reading)").
narrative_ontology:topic_domain(animal_status_kernel__property_reading, "moral philosophy / animal ethics / legal theory").

domain_priors:requires_active_enforcement(animal_status_kernel__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__property_reading, '15ba4679-893a-441e-8cf8-34ed1219da6c').
narrative_ontology:cs_kernel_codification('15ba4679-893a-441e-8cf8-34ed1219da6c', distributed).
narrative_ontology:cs_authority_grounding('15ba4679-893a-441e-8cf8-34ed1219da6c', distributed).
narrative_ontology:cs_reading_relation('15ba4679-893a-441e-8cf8-34ed1219da6c', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('15ba4679-893a-441e-8cf8-34ed1219da6c', animal_status_kernel__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('15ba4679-893a-441e-8cf8-34ed1219da6c', foundational, economic_value_is_sole_relevant_value).
narrative_ontology:cs_axiom_status(economic_value_is_sole_relevant_value, holdable).
narrative_ontology:cs_axiom_grounding('15ba4679-893a-441e-8cf8-34ed1219da6c', economic_value_is_sole_relevant_value, conventional).
narrative_ontology:cs_axiom('15ba4679-893a-441e-8cf8-34ed1219da6c', foundational, moral_considerability_derives_from_ownership).
narrative_ontology:cs_axiom_status(moral_considerability_derives_from_ownership, holdable).
narrative_ontology:cs_axiom_grounding('15ba4679-893a-441e-8cf8-34ed1219da6c', moral_considerability_derives_from_ownership, conventional).
narrative_ontology:cs_reference_frame('15ba4679-893a-441e-8cf8-34ed1219da6c', common_law_chattel_status).
narrative_ontology:cs_drift_state('15ba4679-893a-441e-8cf8-34ed1219da6c', contemporary_sentience_legislation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('15ba4679-893a-441e-8cf8-34ed1219da6c', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__property_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, livestock_industry_owners).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, animal_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, fur_and_entertainment_industries).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, pet_breeding_industry).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, owned_animals).
narrative_ontology:constraint_victim(animal_status_kernel__property_reading, animal_welfare_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status_kernel__property_reading, consumers_of_animal_products).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, property_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__property_reading, economic_value_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own animals as capital assets and extract economic value through breeding, confinement, and slaughter operations. Lobby to keep anti-cruelty statutes narrow (protecting only owner property value, not animal interests) and to keep welfare-based challenges out of courts. Face essentially no legal constraint on use decisions provided the animal's market value as property is not wantonly destroyed.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, livestock_industry_owners, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, livestock_industry_owners, agenda_setter).

% Purchase and use animals as experimental instruments under regulatory frameworks (e.g. IACUC review) whose function is procedural compliance and institutional liability management rather than animal interest protection. Benefit directly from the property framing, which treats research subjects as consumable inputs.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_research_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Use animals for pelts, performance, or display as revenue-generating property. Relocate operations across jurisdictions with weaker anti-cruelty enforcement when domestic constraints tighten; the property framing gives them near-total discretion over disposal and use.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, fur_and_entertainment_industries, beneficiary,
    organized, biographical, mobile, global).

% Breeds and sells animals as commodities; the property frame permits selective breeding for market traits regardless of resulting welfare cost to the animal, since the animal's interests carry no independent legal weight.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, pet_breeding_industry, beneficiary,
    moderate, biographical, mobile, national).

% Bear the full cost of the arrangement — confinement, use, and disposal decisions made entirely by owners. Have no standing to bring claims; anti-cruelty statutes that exist are enforced to protect owner property value (a mutilated or neglected animal is a diminished asset) rather than the animal's own interests. Cannot exit the relationship by any means available to them; the constraint is total and lifelong.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, owned_animals, payer,
    powerless, immediate, trapped, local).

% Litigate and lobby for recognition of animal interests but are structurally locked out of standing under the property reading — courts routinely dismiss claims because animals, as property, cannot be represented as injured parties in their own right. Bear the reputational and resource cost of a losing legal strategy under the kernel as currently read.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, animal_welfare_advocates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status_kernel__property_reading, animal_welfare_advocates, excluded).

% Purchase meat, dairy, leather, and entertainment services at prices that do not internalize any cost attributable to animal interests, since none are legally recognized. Face no structural barrier to continuing consumption; exit (e.g. veganism) is a personal choice, not a constraint-imposed necessity.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, consumers_of_animal_products, beneficiary,
    moderate, immediate, mobile, national).

% Author and interpret anti-cruelty statutes and property law that jointly constitute the kernel's property reading. Could, by statute or precedent, shift standing rules or interest-recognition doctrine, but have historically deferred to property-doctrine continuity and economic-interest arguments from industry.
narrative_ontology:constraint_stakeholder(animal_status_kernel__property_reading, legislators_and_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status_kernel__property_reading, livestock_industry_owners).
narrative_ontology:fixing_cost_class(animal_status_kernel__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable legal framework for owning, transferring, breeding, and disposing of animals as assets — enabling markets in livestock, research subjects, companion animals, and animal products to function with clear title and enforceable contracts.
% TRANSFER_FUNCTION: Moves the entire cost of use, confinement, and disposal onto the animal (who has no legal interest to weigh against the owner's use) while channeling all economic value generated by that use to the owner, purchaser, or downstream commercial actor.
% ABSENT_VOICES: The animals themselves have no voice by construction — the property reading's core move is denying them any interest-bearing legal status. Animal welfare advocates attempt to speak on their behalf but are repeatedly denied standing in court precisely because the kernel, read this way, treats the animal as an object rather than a party capable of being wronged.
% DISAPPEARANCE_RATIONALE: If the property reading vanished overnight and animals gained even minimal interest-bearing legal status, entire industries built on unrestricted use rights (factory farming, fur, certain research paradigms) would face immediate cost restructuring, standing would open for welfare litigation, and markets that currently price animals purely as capital assets would have to internalize interest-based constraints — a substantial reorganization of agricultural, research, and entertainment economics.
% FOUNDING_PROBLEM: Historically, property law over animals solved a genuine coordination problem: establishing clear, transferable title over livestock and working animals so that agrarian and early industrial economies could function — who owns this ox, who is liable if it damages a neighbor's field, who may sell it.
% FOUNDING_PROBLEM_CORROBORATION: Livestock and research industry representatives attest the property framework remains necessary for functioning markets and scientific progress. Independent legal scholars (e.g. animal law academics outside advocacy organizations) and comparative law analyses from jurisdictions that have introduced sentience-recognition statutes (New Zealand, Quebec) attest that the original title-clarity problem is now largely solved by contract and commercial law generally, and that the property reading's persistence beyond that function serves economic interests rather than any remaining coordination need.
narrative_ontology:disappearance_verdict(animal_status_kernel__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status_kernel__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__property_reading, 0.91, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is very high (0.91) because the property reading structurally excludes any countervailing interest that could check use — there is no moral or legal weight on the other side of the ledger from the animal's position. Suppression is substantial (0.72) but not maximal: it operates primarily through doctrinal exclusion (denial of standing) rather than overt physical coercion of challengers, though enforcement of property rights against interference (e.g. trespass to rescue) does involve direct coercive force. Theater ratio is low-moderate (0.28) and rising: anti-cruelty enforcement exists and is not pure performance, but an increasing share of its activity is procedural compliance (research IACUC paperwork, industry self-certification) that protects institutional actors from liability rather than protecting animal interests, which is the theatrical layer this reading generates over time. Accessibility collapse is moderate (0.4), reflecting that alternative legal framings (welfare, personhood) are visible and actively litigated, not eliminated — this reading persists through active doctrinal defense, not through the alternatives being unthinkable.
 *
 * PERSPECTIVAL GAP:
 *   From the owner/agenda-setter seat, this looks like a functioning rope: clear title, predictable markets, minimal transaction friction. From the payer seat (the animal, and derivatively the advocates arguing on its behalf), the same structure is a tangled rope at best — genuine coordination (functioning property markets) riding on asymmetric extraction with no exit for the party who bears the cost. The engine's per-seat computation should reflect this: the beneficiary seats classify the arrangement far more benignly than the payer seats, and that divergence is the data point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (livestock owners, research institutions, entertainment/fur industries, breeders, consumers) sit near the full-beneficiary end of directionality: the constraint subsidizes their economic activity by removing any interest-based cost they would otherwise have to internalize. Owned animals sit at the full-target extreme: trapped exit, no legal voice, and the entire cost of the arrangement falls on them with no derived benefit. Animal welfare advocates are also targets in an unusual sense — they pay in resources and repeated legal defeat because the kernel's own doctrine forecloses their preferred remedy (standing), even though they are not the ones being used as property.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (clear title over livestock and working animals for agrarian/commercial coordination) is largely solved by ordinary commercial and contract law generally; the property reading's persistence beyond that narrow coordination function — specifically its insistence that economic value is the ONLY relevant value — looks like mandate that has outlived its original justification and now primarily serves industries that benefit from the absence of any countervailing interest standard. This is exactly the kind of founding-problem/disappearance-verdict mismatch (status: contested/dead-leaning, verdict: world_rearranges) the R5 genealogy check is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_natural_or_constructed,
    'Is animal-as-property a naturalized default (an unmarked baseline no one chose) or an actively constructed and defended legal doctrine that could be otherwise?',
    'Comparative legal history: jurisdictions that have introduced sentience-recognition statutes (New Zealand''s Animal Welfare Amendment Act, Quebec''s Civil Code amendment) demonstrate the property default is a policy choice, not a logical necessity — if such reforms produce coherent, functioning legal systems, the naturalness claim is falsified.',
    'If constructed rather than natural, the property reading''s persistence is better explained by the concentrated economic interests it serves (a tangled-rope or FSM-adjacent structure) than by any structural inevitability; if natural, the classification should weight toward mountain-adjacent inertia rather than active extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_status_natural_or_constructed, conceptual, 'Whether animal property status is a natural legal baseline or an actively maintained construction.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Does the property reading merely coexist with the welfare and abolitionist readings as competing legal positions, or does its core premise (animals as property with no independent interest) logically foreclose the abolitionist reading''s core premise (animals as persons with a basic right not to be property) within any single legal framework?',
    'Doctrinal analysis: a jurisdiction cannot simultaneously hold that animals are fully alienable property AND that they possess a basic right not to be owned — these are logically incompatible within one framework, unlike the welfare reading, which can be held alongside elements of either extreme as a matter of degree.',
    'This determines whether the property_reading -> abolitionist_reading edge in cs_structure.reading_relations should be forecloses rather than coexists_with; the choice materially affects how the engine models the kernel''s internal contestation structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether the property and abolitionist readings can coexist within one legal framework or are mutually exclusive.').

omega_variable(
    anticruelty_statute_beneficiary_ambiguity,
    'Do existing anti-cruelty statutes represent a genuine (if minimal) countervailing interest for the animal, or are they fully explained as property-value protection for the owner?',
    'Case law analysis: examine enforcement patterns — are anti-cruelty prosecutions ever brought against an owner who is legally entitled to destroy their own property value (suggesting a genuine animal-interest floor), or exclusively in cases involving third-party interference or fraud against the owner''s asset value?',
    'If anti-cruelty statutes never protect against an owner''s own diminishment of their asset, the property reading''s extractiveness is even higher than authored (approaching a pure snare with no countervailing floor at all); if some genuine floor exists even under this reading, extractiveness should be revised slightly downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anticruelty_statute_beneficiary_ambiguity, empirical, 'Whether anti-cruelty enforcement ever protects animal interest independent of owner property value.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__property_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(anim_tr_t8, animal_status_kernel__property_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(anim_tr_t16, animal_status_kernel__property_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(anim_tr_t24, animal_status_kernel__property_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(anim_tr_t32, animal_status_kernel__property_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__property_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__property_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement(anim_be_t8, animal_status_kernel__property_reading, base_extractiveness, 8, 0.85).
narrative_ontology:measurement(anim_be_t16, animal_status_kernel__property_reading, base_extractiveness, 16, 0.88).
narrative_ontology:measurement(anim_be_t24, animal_status_kernel__property_reading, base_extractiveness, 24, 0.89).
narrative_ontology:measurement(anim_be_t32, animal_status_kernel__property_reading, base_extractiveness, 32, 0.9).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__property_reading, base_extractiveness, 40, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__property_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(anim_su_t8, animal_status_kernel__property_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(anim_su_t16, animal_status_kernel__property_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(anim_su_t24, animal_status_kernel__property_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(anim_su_t32, animal_status_kernel__property_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__property_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status_kernel__property_reading, 0.1).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__property_reading, animal_status_kernel__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the animal_status_kernel. property_reading (this file) authors high extractiveness with no countervailing interest recognized for animals. welfare_reading authors moderate extractiveness with property status retained but constrained by regulated welfare obligations. abolitionist_reading authors near-total extractiveness for the same standing arrangement (property status itself is treated as the injustice), but reaches a categorically different prescriptive verdict. Each reading has its own ε, beneficiary/victim set, and classification per the ε-invariance principle; do not average or reconcile across the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
