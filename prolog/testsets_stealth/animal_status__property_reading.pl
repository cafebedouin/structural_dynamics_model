% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Property-Status Regime (Property Reading)
 *   domain: applied ethics/legal philosophy/political economy
 *
 * SUMMARY:
 *   The standing arrangement under contest: animals are legal objects —
 *   property — without independent moral standing, and human ownership is
 *   unrestricted except where welfare statutes bind owners. This file
 *   instantiates the property_reading of the animal_status kernel and nothing
 *   else: within this reading's own lights, the arrangement is the natural,
 *   default condition of human-animal relations, pre-political in origin,
 *   requiring no justification beyond the personhood criterion it applies.
 *   The epsilon referent is the standing arrangement itself, assessed by this
 *   reading's own lights — hence near-zero (0.05): nothing with standing is
 *   extracted from, and the only frictions the reading registers are
 *   welfare-statute burdens on owners and ordinary property-dispute costs.
 *   The sibling readings (animal_status__welfare_reading,
 *   animal_status__abolitionist_reading) are separate constraint stories over
 *   the SAME referent; they differ structurally in the victim set they
 *   declare, not in the arrangement they describe. The claim/metric posture
 *   is deliberate and independent: claimed_type mountain is the reading's own
 *   naturality claim, while the metrics describe the arrangement's actual
 *   operation — including real coercion at the margins and real resistance —
 *   and the engine evaluates the divergence, including the false-summit
 *   signature that a mountain-with-beneficiaries presents.
 *
 * KEY AGENTS:
 *   - - livestock_producers: primary beneficiary (institutional/constrained) — operates the largest share of animal use; the classification makes inventory, facilities, and practices matters of private discretion
 *   - - biomedical_research_institutions: beneficiary (institutional/constrained) — uses animals as standard experimental material with standing questions removed from protocol review
 *   - - companion_animal_owners: beneficiary (moderate/mobile) — acquires, keeps, and ends animals' lives at discretion above anti-cruelty floors; exit is trivially available
 *   - - working_and_entertainment_animal_industries: beneficiary (organized/constrained) — deploys animals as depreciable assets in labor, racing, and performance niches
 *   - - animal_product_consumers: beneficiary (organized/mobile) — buys products priced without charge for the sources' standing; pays externalities indirectly
 *   - - legislative_and_judicial_property_authorities: agenda_setter (institutional/constrained) — defines and administers the person/property boundary through codes and precedent
 *   - - animal_advocacy_movements: excluded (organized/mobile) — contests the classification from outside its decision structure; heard only after translation into property or statutory terms
 *   - - animals_under_ownership: excluded, non-agent in this reading's ontology (powerless/trapped) — bred, confined, used, and killed as chattels; appear in proceedings only as objects of dispute
 *   - - applied_ethics_observers: analytical observer (analytical/analytical) — maps the structure without stake in its continuation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.25).
domain_priors:theater_ratio(animal_status__property_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, mountain).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Property-Status Regime (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied ethics/legal philosophy/political economy").

domain_priors:requires_active_enforcement(animal_status__property_reading).
domain_priors:emerges_naturally(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '6b587721-8f5e-437a-9c11-5e1405c8c98f').
narrative_ontology:cs_kernel_codification('6b587721-8f5e-437a-9c11-5e1405c8c98f', formalized).
narrative_ontology:cs_authority_grounding('6b587721-8f5e-437a-9c11-5e1405c8c98f', lineage).
narrative_ontology:cs_interpretation_layer_present('6b587721-8f5e-437a-9c11-5e1405c8c98f').
narrative_ontology:cs_reading_relation('6b587721-8f5e-437a-9c11-5e1405c8c98f', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b587721-8f5e-437a-9c11-5e1405c8c98f', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('6b587721-8f5e-437a-9c11-5e1405c8c98f', foundational, legal_personhood_requires_rational_moral_agency).
narrative_ontology:cs_axiom_status(legal_personhood_requires_rational_moral_agency, holdable).
narrative_ontology:cs_axiom_grounding('6b587721-8f5e-437a-9c11-5e1405c8c98f', legal_personhood_requires_rational_moral_agency, empirically_contingent).
narrative_ontology:cs_axiom('6b587721-8f5e-437a-9c11-5e1405c8c98f', foundational, ownership_of_nonpersons_unrestricted_by_default).
narrative_ontology:cs_axiom_status(ownership_of_nonpersons_unrestricted_by_default, holdable).
narrative_ontology:cs_axiom_grounding('6b587721-8f5e-437a-9c11-5e1405c8c98f', ownership_of_nonpersons_unrestricted_by_default, conventional).
narrative_ontology:cs_axiom('6b587721-8f5e-437a-9c11-5e1405c8c98f', secondary, welfare_limits_are_owner_discretion_not_entitlements).
narrative_ontology:cs_axiom_status(welfare_limits_are_owner_discretion_not_entitlements, holdable).
narrative_ontology:cs_axiom_grounding('6b587721-8f5e-437a-9c11-5e1405c8c98f', welfare_limits_are_owner_discretion_not_entitlements, conventional).
narrative_ontology:cs_reference_frame('6b587721-8f5e-437a-9c11-5e1405c8c98f', roman_common_law_property_continuity).
narrative_ontology:cs_drift_state('6b587721-8f5e-437a-9c11-5e1405c8c98f', contemporary_animal_law_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6b587721-8f5e-437a-9c11-5e1405c8c98f', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, livestock_producers).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, companion_animal_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, working_and_entertainment_animal_industries).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_product_consumers).
narrative_ontology:constraint_vindicates(animal_status__property_reading, rational_moral_agency_criterion_for_legal_personhood).
narrative_ontology:constraint_vindicates(animal_status__property_reading, owner_liberty_default_in_property_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raise, finish, transport, and slaughter animals at industrial scale. The classification makes herds, facilities, and practices matters of private discretion bounded only by statutes that the industry frequently helps draft, and it converts living animals into balance-sheet inventory. Leaving the arrangement would strand sunk capital, processing infrastructure, and contracted supply chains, so participation continues across generations.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, livestock_producers, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__property_reading, livestock_producers, agenda_setter).

% Purchase, house, procedure, and dispose of animals as standard experimental material. Because the sources of this material hold no standing, protocol review turns on institutional and funder requirements rather than on any claim the subjects might make. Replacement technologies exist in partial form but transition is slow and competes against established funded practice.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, biomedical_research_institutions, beneficiary,
    institutional, biographical, constrained, global).

% Acquire, keep, breed, and end the lives of companion animals at their own discretion above anti-cruelty floors. What flows to them is companionship, security, and status at prices set without any charge for the animals' own position. Exit is trivially available — one simply stops acquiring — yet the arrangement subsidizes continued participation through veterinary, licensing, and market infrastructure.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, companion_animal_owners, beneficiary,
    moderate, biographical, mobile, global).

% Deploy animals for labor, transport, racing, hunting, and performance. The classification keeps their workforce as depreciable, insurable, transferable assets rather than as parties with claims. The sector is narrower than agriculture but concentrated, and its business models assume the asset treatment continues.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, working_and_entertainment_animal_industries, beneficiary,
    organized, biographical, constrained, regional).

% Buy meat, dairy, leather, and tested goods priced without any component reflecting the standing of their sources. Benefits arrive at every purchase; costs arrive indirectly as health and ecological externalities that retail prices rarely carry. Individual exit is a substitution decision; collective exit would require the production system to reorganize first.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_product_consumers, beneficiary,
    organized, immediate, mobile, global).

% Define and administer the person/property boundary through codes, precedent, and doctrine. Challenges to the boundary arrive as filings and bills and are absorbed by routing them into welfare refinement or procedural dismissal. Redrawing the boundary wholesale would unwind settled doctrine across contract, tort, criminal, and family law at once, so administration proceeds incrementally by design.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legislative_and_judicial_property_authorities, agenda_setter,
    institutional, civilizational, constrained, national).

% Contest the classification through litigation, ballot initiatives, documentation, and open rescue. The framework receives their claims only after translation into property disputes or statutory-violation allegations — never as the standing claims they intend to press. They operate freely as associations but cannot reach the decision structure except through instruments the structure itself defines.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_advocacy_movements, excluded,
    organized, generational, mobile, global).

% Are bred, confined, transported, used, and killed as chattels. They appear in legal proceedings only as objects of the dispute — the thing owned, injured, or seized — never as a party to it. Every limit on their treatment arrives at another party's discretion: an owner's forbearance, a legislature's statute, a prosecutor's charging choice. Listed for completeness of the situation; in this reading's ontology they are not agents and derive no directional weight.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animals_under_ownership, excluded,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(animal_status__property_reading, animals_under_ownership).

% Map which parties the classification binds, what it forecloses, and where its justifications strain against adjacent doctrine and cognitive science. Hold no stake in the arrangement's continuation and collect nothing from its operation; their output is analysis consumed by the other seats or by none.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, applied_ethics_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__property_reading, livestock_producers).
narrative_ontology:fixing_cost_class(animal_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves human-human conflicts over a class of beings everyone uses: it fixes ownership boundaries, transfer, theft, and liability for animals once, centrally, through settled doctrine, instead of relitigating each animal's status in every dispute.
% TRANSFER_FUNCTION: Concentrates exclusive control over animals' bodies, labor, and reproductive output in their owners and channels the resulting products through commerce; because the sources of this control hold no standing in this framework, no compensating flow runs in the other direction.
% ABSENT_VOICES: Animals are the structurally absent voice: the reading itself constitutes them as objects, so no seat represents their interests except at owner discretion or legislative beneficence. Animal advocacy movements stand outside the decision structure and are heard only after their claims are translated into property or statutory terms. Future generations bearing the ecological externalities of intensive animal agriculture are likewise unrepresented in the arrangement's administration.
% DISAPPEARANCE_RATIONALE: If the classification vanished overnight, every use relationship built on it loses its substrate: ownership, sale, secured lending, insurance, liability, research protocols, and the food system's inventory logic would all need re-founding around some new account of what animals are. Agriculture, biomedicine, and companion-animal markets would reorganize within years, and the courts' dockets would fill with the transition.
% FOUNDING_PROBLEM: Early law needed a settled answer for beings humans had already domesticated and moved to market: whose animal is this, who answers when an ox injures a neighbor, can a creditor seize a herd. Classifying animals as property answered all three questions with tools the law already possessed.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and property-law treatises outside the benefiting parties corroborate the dispute-resolution origin: Roman liability actions and common-law cattle-trespass and seisin cases long precede any welfare concern, and modern court dockets corroborate that ownership and liability questions over animals remain live daily business. No source outside the tradition itself corroborates the further claim that the classification is morally complete — that claim is attested only from within the beneficiary-aligned tradition, and its lack of external attestation is itself signal.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status__property_reading),
    narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.05 is reading-indexed: within the property reading, no bearer of standing exists from whom the arrangement could extract, so the residual score covers only welfare-statute burdens on owners and dispute frictions — and the temporal series is correspondingly flat. Suppression 0.25 is descriptively honest rather than reading-flattered: the arrangement is mostly self-executing through settled doctrine, but it does deploy real coercion at its margins (trespass and open-rescue prosecutions, agricultural-gag statutes, interference liability), and the suppression_requirement series is authored precisely because enforcement capacity measurably hardened from the 1990s onward as interference attempts grew — a genuine enforcement-trajectory dynamic, not a static picture. Theater 0.12 and rising: a growing symbolic layer (humane-certification labels, exemption-ridden slaughter acts) performs concern while leaving use intact, though from this reading's lights much of it is sincere beneficence. Accessibility_collapse 0.80: within the framework, once the personhood criterion is accepted, no alternative status for animals is articulable — the alternatives are category errors by the framework's own grammar. Resistance 0.30: abolitionist litigation, ballot initiatives, and direct action exist and are processed, but remain marginal to doctrine. All three series share one time grid (1900/1930/1960/1990/2010/2025) so no metric is sampled against another's end-state. Claim and metrics are independent authored facts: the mountain claim is what this reading asserts; the metrics are what the arrangement's operation shows; the engine owns the verdict.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. Commercial owner seats experience the arrangement as their operating substrate — liberty bounded only by statutes many helped draft. Companion owners, equally beneficiaries, experience near-total liberty with trivially available exit, and so register welfare-statute creep as their only friction. The agenda_setter seat experiences settled doctrine requiring routine administration, with challenges absorbed by routing them into welfare refinement. The excluded seats experience closure: advocacy movements find their claims translatable only into property or statutory terms, and the non-agent seat — by the reading's own constitution of it — cannot appear at all. Same-level divergence: livestock producers and companion owners hold the same nominal beneficiary position, but capital-sunk constrained exit versus mobile exit means welfare-statute tightening bites the former and merely annoys the latter — identical global standing, different exposure.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared parties with standing sit on the beneficiary side of the arrangement, so derived directionality clusters near the subsidized end; the five beneficiary declarations drive low d and damped effective extraction for every seated actor. The empty victim set is not an omission — it is this reading's structural signature, and it is the precise element over which the sibling readings diverge. Animals are authored as a stakeholder with agent:false: the reading constitutes them as objects, so they must not feed the directionality computation as if they collected from or paid into the arrangement — encoding them otherwise would smuggle a sibling reading's ontology into this file. Observers carry analytical atoms and no directional weight. On the receipt surface: the arrangement's economic value demonstrably concentrates with commercial agricultural producers, so gain_flow names livestock_producers rather than diffuse. Fixing cost is prohibitive for the seat that could fix it: redrawing the person/property boundary would unwind doctrine across contract, tort, criminal, and family law simultaneously, a cost the authorities bear directly against benefits they do not concede.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling human-human disputes over beings humans had already domesticated and moved to market — remains live: ownership, liability, and seizure dockets process animal-property questions daily, so founding_problem_status is live and the live-times-world_rearranges cell raises no zombie flag. No mandatrophy is declared. What the classification prevents here is a subtler error than mislabeled extraction: it prevents this reading's near-zero epsilon from being read as a verdict about the arrangement rather than about the reading. The corpus-level measurement is the epsilon spread across the three sibling files over one fixed referent; this file contributes the low anchor of that spread. The open trajectory question — absorption versus dissolution — is carried by the welfare_statute_trajectory omega rather than forced into the type claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_natural_vs_constructed_status,
    'Is the classification of animals as legal objects a natural feature of the human-animal relationship (pre-political, arising from domestication and dependence), or a constructed legal choice that identifiable beneficiary industries maintain?',
    'Comparative legal anthropology and history: examine societies lacking strong property-in-animals concepts, medieval eras when animals were tried as defendants, and modern moments when the boundary was actively renegotiated (personhood habeas petitions, welfare-statute expansion). If the boundary moves when beneficiary interests are at stake and holds when they are not, construction is indicated.',
    'If constructed-with-beneficiaries, the mountain claim fails false-summit evaluation and the arrangement reclassifies toward the coordinated-and-collecting family, with commercial owner seats as the coordinated-and-collecting side; if genuinely natural, the mountain certification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_natural_vs_constructed_status, empirical, 'Whether animal property status is natural law or maintained construct').

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the property_reading of the animal_status kernel; would the welfare_reading or abolitionist_reading of the same kernel, over the same standing arrangement, produce a different victim set and a different epsilon?',
    'Generate the sibling stories (animal_status__welfare_reading, animal_status__abolitionist_reading) and compare their victim sets and epsilon values against this file''s empty victim set and epsilon 0.05.',
    'The disagreement is located entirely in the standing element — who, if anyone, belongs in the victim set. Sibling readings share this story''s referent and differ only in what counts as a party; the spread of epsilon values across the family is the measurement the kernel contest exists to take.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of the animal_status kernel').

omega_variable(
    welfare_statute_trajectory,
    'Do expanding welfare statutes represent the arrangement absorbing its critics (refining property status and stabilizing it) or the first stage of its dissolution (a transitional phase in which the status migrates)?',
    'Track whether welfare reforms plateau at use-preserving levels (humane slaughter with exemptions, enrichment mandates) or begin eliminating entire use classes outright; watch whether any jurisdiction grants standing or personhood to any animal class.',
    'Absorption supports the stable-mountain reading of this file; dissolution supports reclassification toward a transitional-support type whose justification is the migration itself rather than the steady state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statute_trajectory, empirical, 'Whether welfare expansion stabilizes or dissolves the property arrangement').

omega_variable(
    standing_criterion_selectivity,
    'Is the exclusion of animals from legal standing a principled application of the rational-moral-agency criterion for personhood, or a post hoc rationalization of an economically valuable practice?',
    'Test the criterion for consistency across adjacent cases: corporations granted personhood without rational agency, rivers and idols granted standing in some jurisdictions, fetal and AI entities contested. Selective application where animal interests conflict with human use indicates the criterion serves the beneficiary structure.',
    'Consistent application supports the principled-mountain reading; selective application indicates the naturality claim is cover, strengthening the constructed-constraint resolution of the FSM omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_criterion_selectivity, conceptual, 'Whether the personhood criterion is principle or rationalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 1900, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1900, animal_status__property_reading, theater_ratio, 1900, 0.04).
narrative_ontology:measurement(anim_tr_t1930, animal_status__property_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(anim_tr_t1960, animal_status__property_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(anim_tr_t1990, animal_status__property_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(anim_tr_t2010, animal_status__property_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(anim_tr_t2025, animal_status__property_reading, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(anim_be_t1900, animal_status__property_reading, base_extractiveness, 1900, 0.03).
narrative_ontology:measurement(anim_be_t1930, animal_status__property_reading, base_extractiveness, 1930, 0.04).
narrative_ontology:measurement(anim_be_t1960, animal_status__property_reading, base_extractiveness, 1960, 0.04).
narrative_ontology:measurement(anim_be_t1990, animal_status__property_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(anim_be_t2010, animal_status__property_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(anim_be_t2025, animal_status__property_reading, base_extractiveness, 2025, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1900, animal_status__property_reading, suppression_requirement, 1900, 0.05).
narrative_ontology:measurement(anim_su_t1930, animal_status__property_reading, suppression_requirement, 1930, 0.06).
narrative_ontology:measurement(anim_su_t1960, animal_status__property_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(anim_su_t1990, animal_status__property_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(anim_su_t2010, animal_status__property_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(anim_su_t2025, animal_status__property_reading, suppression_requirement, 2025, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the legal status of animals' decomposes into three structurally distinct constraints — one per reading of the animal_status kernel — per the epsilon-invariance principle. Each reading authors its own epsilon over the SAME referent (the standing instrumental-use arrangement): this file, the property_reading, authors epsilon 0.05 with an empty victim set; the welfare_reading authors higher epsilon with sentient-interest victims; the abolitionist_reading authors the highest epsilon with rights-holder victims. The property reading is upstream in the family: welfare statutes amend property status without replacing it, and abolitionist litigation argues from the demonstrated inadequacies of both predecessors. Edges therefore run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
