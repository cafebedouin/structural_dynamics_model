% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Property Status Regime (Property Reading)
 *   domain: applied ethics/legal philosophy/political economy
 *
 * SUMMARY:
 *   This story authors ONE reading of the animal_status kernel: the property
 *   reading, under which animals are legal objects, ownership is unrestricted
 *   except by welfare statutes, and the arrangement is assessed by the
 *   reading's own lights. The epsilon referent is the standing arrangement
 *   under contest — animals-as-property with the welfare overlay — never any
 *   alternative arrangement this reading might prefer. Within those lights
 *   the arrangement exhibits near-zero extractiveness (no victim set exists:
 *   the governed class lacks standing by definition, and remaining frictions
 *   are ordinary human-to-human property disputes), genuine coordination
 *   function (title, transfer, collateral, dispute resolution), and low
 *   suppression (participants are volunteers; exclusion of non-owners is
 *   constitutive of property rather than coercive overhead). Per the
 *   claim/metric independence rule, claimed_type (rope) and the metrics were
 *   authored separately, each as believed true; the engine computes per-seat
 *   classifications from the structural data. KEY AGENTS (by structural
 *   relationship): livestock_producers — primary beneficiary
 *   (institutional/mobile); biomedical_research_institutions — beneficiary
 *   with constrained exit (institutional/constrained);
 *   companion_animal_owners — diffuse small-holder beneficiaries
 *   (moderate/mobile); legislatures_and_judiciaries — agenda setter
 *   administering the classification and its welfare overlay
 *   (institutional/constrained); animal_advocacy_organizations — excluded
 *   seat, denied any procedural route around owner-defendants
 *   (organized/trapped); legal_philosophers — analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.15).
domain_priors:theater_ratio(animal_status__property_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Property Status Regime (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied ethics/legal philosophy/political economy").

domain_priors:requires_active_enforcement(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '38d6eec2-fe6f-472d-ab23-a491b5de4966').
narrative_ontology:cs_kernel_codification('38d6eec2-fe6f-472d-ab23-a491b5de4966', formalized).
narrative_ontology:cs_authority_grounding('38d6eec2-fe6f-472d-ab23-a491b5de4966', lineage).
narrative_ontology:cs_interpretation_layer_present('38d6eec2-fe6f-472d-ab23-a491b5de4966').
narrative_ontology:cs_reading_relation('38d6eec2-fe6f-472d-ab23-a491b5de4966', animal_status__welfare_reading, influences).
narrative_ontology:cs_reading_relation('38d6eec2-fe6f-472d-ab23-a491b5de4966', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('38d6eec2-fe6f-472d-ab23-a491b5de4966', foundational, moral_standing_tracks_personhood_not_sentience).
narrative_ontology:cs_axiom_status(moral_standing_tracks_personhood_not_sentience, holdable).
narrative_ontology:cs_axiom_grounding('38d6eec2-fe6f-472d-ab23-a491b5de4966', moral_standing_tracks_personhood_not_sentience, deontological).
narrative_ontology:cs_axiom('38d6eec2-fe6f-472d-ab23-a491b5de4966', secondary, welfare_duties_bind_owners_not_animals).
narrative_ontology:cs_axiom_status(welfare_duties_bind_owners_not_animals, holdable).
narrative_ontology:cs_axiom_grounding('38d6eec2-fe6f-472d-ab23-a491b5de4966', welfare_duties_bind_owners_not_animals, conventional).
narrative_ontology:cs_reference_frame('38d6eec2-fe6f-472d-ab23-a491b5de4966', roman_law_res_division).
narrative_ontology:cs_drift_state('38d6eec2-fe6f-472d-ab23-a491b5de4966', contemporary_welfare_statute_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('38d6eec2-fe6f-472d-ab23-a491b5de4966', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, livestock_producers).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, companion_animal_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raise, breed, confine, transport, and slaughter animals as production inventory. The property classification lets them buy, sell, collateralize, and cull animal assets under ordinary commercial law; welfare statutes add per-operation compliance costs that industry associations help draft. Exit is real but costly: capital converts to other commodities or to jurisdictions with thinner statutes.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, livestock_producers, beneficiary,
    institutional, generational, mobile, global).

% Use animals as experimental material under institutional protocols. Property status makes acquisition, housing, and euthanasia procurement matters of asset management; welfare statutes layer protocol review and inspection on top. Replacement methods exist for some research lines but not others, so full exit from animal use is unavailable for parts of the research program.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Acquire, keep, breed, and rehome animals as personal property. Title gives them exclusive decision rights over veterinary care, diet, confinement, breeding, and end of life; anti-cruelty statutes bound the extremes. Exit is trivial — stop acquiring — and the arrangement costs them little beyond purchase price and whatever voluntary standards they choose to follow.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, companion_animal_owners, beneficiary,
    moderate, biographical, mobile, national).

% Enact and interpret the classification: civil codes place animals among goods or things, courts resolve ownership and damage disputes, and legislatures append welfare statutes that restrict specific practices while leaving title intact. They can amend the welfare overlay by ordinary legislative process; altering the underlying classification would mean rewriting property foundations that secure credit, insurance, and food-supply chains.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legislatures_and_judiciaries, agenda_setter,
    institutional, generational, constrained, national).

% Campaign for statutory amendments, document conditions undercover, and attempt litigation concerning animals. Every procedural route runs through an owner-defendant: private actions target individual acts rather than the classification, and no procedure exists for bringing a claim on an animal's behalf. Their available levers are persuasion, ballot initiatives, and market campaigns; there is no procedural exit from the standing denial itself.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_advocacy_organizations, excluded,
    organized, biographical, trapped, global).

% Analyze where the person/thing boundary does legal work, where welfare statutes strain it, and what alternative classifications would require. They hold no stake in outcomes and publish arguments both defending and criticizing the property settlement.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__property_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Property law coordinates human access to and control over animals: it assigns exclusive use, enables sale, breeding, inheritance, and collateralization, and settles competing human claims over the same animal without open conflict.
% TRANSFER_FUNCTION: Moves exclusive control of animal bodies, labor, and products to human owners, and moves ownership between humans through markets; welfare statutes move compliance costs onto owners as a condition of retaining title.
% ABSENT_VOICES: Animal advocacy organizations and any would-be animal representative: they would object that the class the arrangement governs has no seat and no proxy with standing, and they exist outside legislatures and courtrooms as campaigning NGOs and litigants denied standing. The governed class itself — the animals — cannot appear in any forum this arrangement recognizes.
% DISAPPEARANCE_RATIONALE: Food systems, research pipelines, companion-animal economies, veterinary consent structures, and credit instruments secured on livestock all presuppose alienable title to animals. Overnight removal would force immediate reclassification of enormous asset stocks and suspend routine agricultural and scientific practice until a replacement framework was written.
% FOUNDING_PROBLEM: How should a legal order built around persons classify living beings that humans use, trade, and kill: recognize them as subjects, leave them outside the law entirely, or attach them to persons as objects? The property solution attached them to owners, making every human-animal question administrable as a question about the owner.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians of the Roman res classification and comparative private-law scholars attest both the recurring classification problem and the property solution from outside the owning industries; codification debates in successive civil-law revisions return to the same problem on the record. No beneficiary attestation is relied on for the genealogy.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.05 because, within this reading, no party bears a transfer: the class that would bear costs lacks standing, and human participants transact voluntarily at market prices. Suppression is 0.15 — low, and noted as a raw unscaled structural property (only extractiveness is scaled by directionality and scope in the engine): the arrangement excludes non-owners as a matter of property's constitutive function rather than coercing participants, who may decline to acquire animals at zero legal cost. Theater_ratio is 0.16 and rising slowly: welfare inspection performs visible oversight while leaving title and core use decisions untouched, so a growing share of the arrangement's activity is demonstrative rather than restrictive. Accessibility_collapse is 0.30 — alternatives (voluntary welfare standards, sanctuaries, non-acquisition, replacement research methods) remain workable once the arrangement is understood. Resistance is 0.30: organized pressure exists and concentrates on amending the statutory overlay rather than displacing the classification, which continues to operate with broad acquiescence among participants. The measurement series run on one shared time grid (t=0..125, anchored approximately to 1900-2025): every tracked metric is authored at every examined time point, so no end-state value is substituted backward. Suppression_requirement series are deliberately omitted: enforcement capacity of the property frame is stable across the interval, and the static picture is carried by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   There are no payer seats in this reading by construction, so the sharpest divergence is between the beneficiary seats and the excluded seat. From the owner and administrator positions the arrangement computes as functioning coordination: title does real work, disputes resolve, welfare statutes are honored costs. From the excluded advocacy seat the same structure registers as a closed door — every objective routes through defendants and legislatures that owe them nothing procedurally. The engine computes these divergent per-seat classifications from power, exit, and role data; the divergence between a rope-shaped computation for owners and a blocked-access computation for advocates is the measurement this story exists to take, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (livestock_producers, biomedical_research_institutions, companion_animal_owners) derive low directionality — the arrangement subsidizes their use-interests and their exits are mobile to constrained. The agenda setter (legislatures_and_judiciaries) sits near symmetric: it administers and collects fees but bears neither the overlay's compliance costs nor the classification's use-value. The excluded advocacy seat derives no directionality from beneficiary/victim data because it appears in neither array; left to the canonical fallback it would be misplaced, so an explicit override sets the organized power atom to d=0.65: the constraint costs that seat its primary objective (any procedural route around owner-defendants) while returning incidental wins through the welfare overlay, placing it well toward the target end despite bearing no monetary transfer. The override is scoped to the organized atom, which only the advocacy seat occupies, so no beneficiary's derived low d is disturbed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how a person-centered legal order classifies the beings humans use — remains live: every codification and every new use class returns to it, and the arrangement still performs its original coordinating work, so there is no mandate decay to resolve and no zombie flag (founding_problem_status=live crossed with disappearance_verdict=world_rearranges yields no mismatch). The classification discipline here cuts both ways. Reading the property frame as pure coordination without recording its premise-dependence would launder a definitional choice (empty victim set) into a structural finding; reading it as extraction would import a sibling reading's premises into this file's epsilon and break epsilon-invariance. The omega apparatus carries the premise-dependence instead: the kernel_reading_commitment omega records that the near-zero epsilon holds only within this reading, and the natural_hierarchy_vs_convention omega records that the reading's own tradition is divided on whether its boundary is discovered or chosen. Receipt-surface note: gain_flow='diffuse' plus fixing_cost='prohibitive' is authored from the facts — gains spread across all owning classes and rewriting the classification means rebuilding property foundations — and the engine may compute the corresponding cell signature; that consequence was accepted, not tuned toward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the property_reading of the animal_status kernel: animals are legal objects without independent standing, so the victim set is empty and extractiveness is near-zero. What would the sibling readings change?',
    'Compare against the sibling files animal_status__welfare_reading and animal_status__abolitionist_reading, which instantiate the same standing arrangement with sentience-based and rights-based victim sets respectively; the disagreement is located precisely in whether sentience confers standing (victim-set membership).',
    'Under either sibling reading the identical arrangement carries animals in the victim set, raising epsilon substantially and shifting the computed classification toward hybrid or extractive types; this file''s near-zero epsilon is valid only within the property reading''s own lights and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the animal_status kernel; sibling readings alter the victim set and hence epsilon.').

omega_variable(
    natural_hierarchy_vs_convention,
    'Within this reading''s own tradition, is the human-exclusive standing boundary a discovered natural limit (species hierarchy as fact) or a constructed legal convention maintained by continuing choice?',
    'Analysis of the reading''s internal defenses: natural-law arguments versus conventionalist arguments; comparative and historical variation in where jurisdictions draw the boundary.',
    'If natural, the arrangement approaches fixed-law character and reform pressure is a category error; if conventional, it is revisable legislation and the near-zero extractiveness rests on an adopted premise rather than a discovered one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_hierarchy_vs_convention, conceptual, 'Whether the standing boundary is treated as natural law or legal convention inside the property tradition.').

omega_variable(
    welfare_overlay_stringency,
    'Where does the welfare-statute overlay sit between cosmetic licensing and effective use-restriction for the major animal-use classes?',
    'Comparative statutory stringency indices and enforcement-outcome data across jurisdictions over the interval.',
    'If statutes harden into de facto prohibitions for major use classes, the arrangement''s suppression component rises and its coordination profile shifts; if they remain thin, the near-zero-extraction profile holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_overlay_stringency, empirical, 'Stringency of the welfare overlay that bounds otherwise-unrestricted ownership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(anim_tr_t25, animal_status__property_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(anim_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(anim_tr_t75, animal_status__property_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement(anim_tr_t100, animal_status__property_reading, theater_ratio, 100, 0.14).
narrative_ontology:measurement(anim_tr_t125, animal_status__property_reading, theater_ratio, 125, 0.16).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(anim_be_t25, animal_status__property_reading, base_extractiveness, 25, 0.03).
narrative_ontology:measurement(anim_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.04).
narrative_ontology:measurement(anim_be_t75, animal_status__property_reading, base_extractiveness, 75, 0.04).
narrative_ontology:measurement(anim_be_t100, animal_status__property_reading, base_extractiveness, 100, 0.05).
narrative_ontology:measurement(anim_be_t125, animal_status__property_reading, base_extractiveness, 125, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(animal_status__property_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the legal status of animals' decomposes into three structurally distinct constraints (epsilon-invariance): the property reading (this file, animals outside the victim set, epsilon ~0.05), the welfare reading (sentience constrains use, animals partially in the victim set), and the abolitionist reading (inherent value precludes use, animals fully in the victim set). This reading is the upstream substrate: welfare statutes regulate owners qua owners and therefore presuppose the property categories, which is why the edge to the welfare reading is directional influence rather than mere coexistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__property_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
