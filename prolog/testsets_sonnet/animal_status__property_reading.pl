% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Property Reading of Animal Legal Status
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the PROPERTY READING of the animal_status kernel:
 *   animals are legal objects, not rights-holders; human ownership is
 *   essentially unrestricted, bounded only by welfare statutes that owners
 *   and industry themselves substantially shape. Under this reading there is
 *   no animal victim class because animals are not parties capable of being
 *   wronged in the legal sense that generates a victim declaration — the
 *   constraint operates entirely as human-to-human property law (title,
 *   transfer, liability, statutory compliance). This is why extractiveness is
 *   authored near zero (~0.05): the coordination function (clear title,
 *   predictable disposal rights, tradeable capital) genuinely resolves a
 *   human coordination problem with minimal coercive overhead falling on any
 *   recognized party. The welfare_reading and abolitionist_reading are
 *   DIFFERENT constraints with different ε values and different victim sets —
 *   they are not alternate measurements of this one constraint, per the
 *   ε-invariance principle. Do not average across readings; this file
 *   describes only the property reading.
 *
 * KEY AGENTS:
 *   - livestock_producers: beneficiary (organized/arbitrage) — hold animals as tradeable capital
 *   - pet_owners: beneficiary (moderate/mobile) — unilateral disposal authority
 *   - animal_research_institutions: beneficiary (institutional/arbitrage) — proceed without animal standing
 *   - commercial_animal_industries: beneficiary+agenda_setter (organized/arbitrage) — shape the welfare-statute floor
 *   - animal_welfare_advocates: excluded (organized/constrained) — no standing to litigate on the animal's behalf
 *   - animals_as_subjects: excluded, non-agent — the entities at issue, without legal personhood under this reading
 *   - courts_and_legislatures: observer (institutional/analytical) — apply the settled property doctrine
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
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Property Reading of Animal Legal Status").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'e112c325-115f-4cb9-a96f-a0b4771554e7').
narrative_ontology:cs_kernel_codification('e112c325-115f-4cb9-a96f-a0b4771554e7', formalized).
narrative_ontology:cs_authority_grounding('e112c325-115f-4cb9-a96f-a0b4771554e7', lineage).
narrative_ontology:cs_interpretation_layer_present('e112c325-115f-4cb9-a96f-a0b4771554e7').
narrative_ontology:cs_reading_relation('e112c325-115f-4cb9-a96f-a0b4771554e7', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('e112c325-115f-4cb9-a96f-a0b4771554e7', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('e112c325-115f-4cb9-a96f-a0b4771554e7', foundational, animals_lack_independent_moral_standing).
narrative_ontology:cs_axiom_status(animals_lack_independent_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('e112c325-115f-4cb9-a96f-a0b4771554e7', animals_lack_independent_moral_standing, deontological).
narrative_ontology:cs_axiom('e112c325-115f-4cb9-a96f-a0b4771554e7', foundational, ownership_title_is_freely_alienable_absent_statute).
narrative_ontology:cs_axiom_status(ownership_title_is_freely_alienable_absent_statute, holdable).
narrative_ontology:cs_axiom_grounding('e112c325-115f-4cb9-a96f-a0b4771554e7', ownership_title_is_freely_alienable_absent_statute, conventional).
narrative_ontology:cs_reference_frame('e112c325-115f-4cb9-a96f-a0b4771554e7', common_law_chattel_doctrine).
narrative_ontology:cs_drift_state('e112c325-115f-4cb9-a96f-a0b4771554e7', contemporary_animal_cognition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e112c325-115f-4cb9-a96f-a0b4771554e7', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, livestock_producers).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, pet_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, commercial_animal_industries).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_title_clarity_doctrine).
narrative_ontology:constraint_vindicates(animal_status__property_reading, freedom_of_use_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own animals as productive assets, make breeding, confinement, and slaughter decisions without needing to justify them against any independent animal interest, subject only to specific welfare statutes (anti-cruelty codes, transport rules). Clear title and predictable liability let them finance, insure, and trade animals as capital.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, livestock_producers, beneficiary,
    organized, generational, arbitrage, national).

% Hold companion animals as property they can relocate, rehome, or euthanize by their own judgment within statutory limits. The property frame gives them unilateral decision authority without needing a court or guardian to weigh the animal's independent interest.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, pet_owners, beneficiary,
    moderate, biographical, mobile, national).

% Use animals in experimentation under institutional review committees that apply welfare standards but do not recognize the animal as a party with standing to object or litigate. This lets research proceed on a cost-benefit basis set entirely by human institutional actors.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_research_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Lobby for and help draft the welfare statutes that define the only constraints on ownership; effectively set the regulatory floor they then operate within. Benefit from a legal category (property) that keeps disputes framed as human-to-human title, liability, and contract questions.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, commercial_animal_industries, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_status__property_reading, commercial_animal_industries, agenda_setter).

% Argue animals have interests that should generate independent legal standing, not merely statutory floors set by owners and industry. They can lobby for stricter welfare statutes but cannot bring claims on behalf of an animal as a rights-holder; their objection is structurally routed around by the property frame itself.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_welfare_advocates, excluded,
    organized, generational, constrained, national).

% Are the entities whose treatment is at issue but hold no legal personhood, cannot initiate proceedings, and are represented (if at all) only through owners, welfare statutes, or advocacy proxies. Listed for narrative completeness as a non-agent entity under this reading — the property reading's core claim is precisely that they are not independent parties to the constraint.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animals_as_subjects, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(animal_status__property_reading, animals_as_subjects).

% Adjudicate ownership disputes between humans and draft/interpret welfare statutes. They apply the property frame as settled law in the vast majority of jurisdictions, treating animal personhood claims as fringe litigation rather than live doctrine.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, low-transaction-cost legal category (property) that lets humans buy, sell, breed, insure, confine, and dispose of animals without case-by-case adjudication of an independent animal interest — the same coordination function ordinary property law provides for any asset.
% TRANSFER_FUNCTION: Moves decision authority and economic value entirely among human parties: sellers to buyers, owners to insurers, producers to consumers. Under this reading nothing is transferred FROM the animal, because the animal is not a party capable of holding or losing anything in the legal sense.
% ABSENT_VOICES: Animal welfare advocates argue for expanded statutory floors or personhood status but are not parties to ownership transactions and have no standing to compel change outside the legislative process; the animals themselves have no voice in any forum under this reading by construction.
% DISAPPEARANCE_RATIONALE: If the property classification vanished overnight and animals acquired independent legal standing, the entire architecture of livestock production, veterinary practice, insurance, research licensing, and pet ownership would require re-litigation from first principles — this is precisely the abolitionist reading's claim, which is why the two readings cannot be held in the same framework simultaneously.
% FOUNDING_PROBLEM: Pre-modern and early-modern legal systems needed a workable category to resolve disputes over valuable animals (livestock theft, breeding rights, working animals, trade) without adjudicating metaphysical questions about animal minds or moral status.
% FOUNDING_PROBLEM_CORROBORATION: Property-law scholars and livestock industry bodies attest the founding problem (efficient allocation and dispute resolution over animal-as-asset) remains live and adequately served by the current statutory-floor model. Animal law scholars and philosophers working outside industry funding attest the founding problem has been overtaken by accumulated evidence of animal sentience and cognition, and that the category now functions to foreclose a moral question rather than merely to solve an allocation problem — this corroboration comes from academic sources with no direct stake in ownership continuity.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at ~0.05 because, under this reading's own terms, there is no party from whom the constraint extracts beyond ordinary property-transaction costs shared by any owner of any asset class. Suppression is low-moderate (0.12) reflecting only the residual friction of welfare-statute compliance costs, not coercion against a recognized rights-holder. Accessibility collapse is low (0.2): alternative legal framings (welfare-interest standing, personhood) remain actively litigated and legislated in multiple jurisdictions, so alternatives have not collapsed. Resistance is moderate (0.3): the reading faces sustained organized opposition from welfare and abolitionist advocates, which is real resistance even though it has not dislodged the doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries are humans who hold, trade, or administer animals as property; the directionality derivation places them near the full-beneficiary end because the property frame subsidizes their decision authority and asset liquidity. There is no victim group declared, because under this reading's structural premise animals are not parties capable of bearing the kind of cost the framework's directionality math tracks — this is the reading's defining structural claim, not an oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The property reading's founding problem (efficient allocation of a valuable, physically distinct asset class) plausibly remains partially live for commercial livestock and working-animal contexts, but the founding_problem_status is authored as contested because a substantial body of independent (non-industry) scholarship argues the doctrine has calcified into premise-foreclosure — using an allocation-efficiency justification to preempt a since-arisen sentience/standing question rather than to solve the original title-dispute problem. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is exactly the signal the R5 consumer is built to catch: the arrangement's persistence value to its own beneficiaries may now exceed its residual coordination value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_natural_or_constructed,
    'Is animal-as-property a naturally emergent legal category tracking a real ontological difference between humans and animals, or a constructed doctrine that benefits identifiable commercial actors by foreclosing a contested moral question?',
    'Comparative legal history across jurisdictions that have granted partial personhood or standing to animals (e.g. some rivers/ecosystems, great ape personhood rulings) and tracking whether commercial actors'' lobbying activity concentrates specifically on preventing standing expansion.',
    'If constructed-and-defended, the property reading functions closer to a false-summit doctrine serving commercial_animal_industries; if genuinely tracking an uncontested ontological line, the near-zero extractiveness is fully warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_status_natural_or_constructed, conceptual, 'Whether the property/non-person line is natural-law-like or an actively defended construction.').

omega_variable(
    kernel_reading_selection_effect,
    'Given that three coherent readings of the animal_status kernel exist (property, welfare, abolitionist) with sharply different ε values and victim sets, what determines which reading a given jurisdiction or observer adopts, and is that selection itself capturable by the beneficiaries of the property reading?',
    'Track legislative and judicial adoption patterns of each reading against industry lobbying expenditure and campaign contribution data in the relevant jurisdiction.',
    'If reading-selection correlates strongly with beneficiary lobbying investment, the apparent legal stability of the property reading is itself a product of asymmetric resource investment rather than settled consensus, which would inform (without changing) this story''s own ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_effect, empirical, 'Whether the choice among sibling kernel readings is itself subject to capture.').

omega_variable(
    welfare_statute_floor_capture,
    'Are welfare statutes (the only constraint this reading recognizes) set at a level reflecting genuine independent ethical deliberation, or substantially at a level commercial_animal_industries themselves lobby for as the agenda_setter secondary role suggests?',
    'Analysis of statutory drafting history, comparing initial advocacy-proposed welfare standards against final enacted statutes and identifying which provisions originated from industry-drafted model legislation.',
    'High correlation with industry-drafted language would support treating the ''welfare statute floor'' as an internally-set ceiling rather than an externally-imposed constraint, sharpening rather than eliminating the near-zero ε reading but explaining why resistance (0.3) exists despite low measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_floor_capture, empirical, 'Whether the statutory floor is independently set or industry-authored.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t8, animal_status__property_reading, theater_ratio, 8, 0.06).
narrative_ontology:measurement(anim_tr_t16, animal_status__property_reading, theater_ratio, 16, 0.07).
narrative_ontology:measurement(anim_tr_t24, animal_status__property_reading, theater_ratio, 24, 0.07).
narrative_ontology:measurement(anim_tr_t32, animal_status__property_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement(anim_tr_t40, animal_status__property_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(anim_be_t8, animal_status__property_reading, base_extractiveness, 8, 0.04).
narrative_ontology:measurement(anim_be_t16, animal_status__property_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement(anim_be_t24, animal_status__property_reading, base_extractiveness, 24, 0.05).
narrative_ontology:measurement(anim_be_t32, animal_status__property_reading, base_extractiveness, 32, 0.05).
narrative_ontology:measurement(anim_be_t40, animal_status__property_reading, base_extractiveness, 40, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_status__property_reading, 0.1).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% Three-member constraint family decomposing the natural-language 'legal status of animals' concept per the ε-invariance principle. property_reading (this file, ε~0.05, no animal victim class), welfare_reading (ε moderate, animals have constrained-but-not-prohibited interests, no animal victim class but genuine statutory limits treated as real constraint), abolitionist_reading (ε high, animals ARE the victim class, instrumental use itself is the extraction). The property reading structurally FORECLOSES the abolitionist reading (rights-holder status and pure-object status cannot coexist in one legal framework) while COEXISTING WITH the welfare reading in practice (most real jurisdictions blend property title with statutory welfare floors, which is why property_reading and welfare_reading are frequently mistaken for the same doctrine rather than decomposed as here).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
