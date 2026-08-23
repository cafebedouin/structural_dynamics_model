% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animals as Legal Objects — Property Reading of the Animal Status Kernel
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This file instantiates the PROPERTY READING of the contested
 *   animal_status kernel: animals are legal objects without independent moral
 *   standing, human ownership is presumptively unrestricted, and the only
 *   operative limits are welfare statutes plus ordinary human-to-human
 *   property law. The epsilon referent is the standing arrangement under
 *   contest — the animals-as-chattels regime — assessed by this reading's own
 *   lights: because the reading recognizes no animal claims, almost nothing
 *   registers as extraction (epsilon 0.05, matching the expected structural
 *   delta), and no victim set exists BY DESIGN of this reading. The sibling
 *   readings — animal_status__welfare_reading and
 *   animal_status__abolitionist_reading — are separate constraint files with
 *   their own epsilon values and victim structures; they are linked here via
 *   network.affects_constraints and cs_structure.reading_relations, never
 *   blended into this one. The claim/metric split is deliberate: the reading
 *   CLAIMS rope (its own self-understanding as neutral coordination
 *   infrastructure) while the metrics are authored descriptively of the
 *   regime's actual operation, including its enforcement history.
 *
 * KEY AGENTS:
 *   - legislative_authorities: agenda-setter (institutional/constrained) — maintains the classification; writes the only permitted limits
 *   - judiciary: agenda-setter (institutional/constrained) — administers via doctrine; denies standing at the threshold
 *   - welfare_enforcement_agencies: agenda-setter (institutional/constrained) — administers the welfare overlay under appropriated budgets
 *   - livestock_agriculture: primary beneficiary (organized/arbitrage) — holds the bulk of animal assets; jurisdictional mobility disciplines regulators
 *   - biomedical_research_sector: beneficiary (institutional/constrained) — infrastructure-bound dependence on the baseline
 *   - companion_animal_keepers: beneficiary (moderate/mobile) — household-level benefit under the same object classification
 *   - animal_product_consumers: incidental beneficiary (moderate/mobile) — cheap outputs, occasional ballot leverage
 *   - animal_advocacy_organizations: excluded voice (organized/constrained) — opposes the arrangement from outside the formal conversation
 *   - animal_law_scholars: analytical observer (analytical/analytical) — documents the doctrine and its inconsistencies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.35).
domain_priors:theater_ratio(animal_status__property_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animals as Legal Objects — Property Reading of the Animal Status Kernel").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '8df1e0b3-93b0-4b27-b91b-dde8069632bf').
narrative_ontology:cs_kernel_codification('8df1e0b3-93b0-4b27-b91b-dde8069632bf', formalized).
narrative_ontology:cs_authority_grounding('8df1e0b3-93b0-4b27-b91b-dde8069632bf', lineage).
narrative_ontology:cs_interpretation_layer_present('8df1e0b3-93b0-4b27-b91b-dde8069632bf').
narrative_ontology:cs_reading_relation('8df1e0b3-93b0-4b27-b91b-dde8069632bf', animal_status__welfare_reading, influences).
narrative_ontology:cs_reading_relation('8df1e0b3-93b0-4b27-b91b-dde8069632bf', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('8df1e0b3-93b0-4b27-b91b-dde8069632bf', foundational, animals_classified_as_chattels_not_persons).
narrative_ontology:cs_axiom_status(animals_classified_as_chattels_not_persons, holdable).
narrative_ontology:cs_axiom_grounding('8df1e0b3-93b0-4b27-b91b-dde8069632bf', animals_classified_as_chattels_not_persons, conventional).
narrative_ontology:cs_axiom('8df1e0b3-93b0-4b27-b91b-dde8069632bf', secondary, owner_discretion_default_rule).
narrative_ontology:cs_axiom_status(owner_discretion_default_rule, holdable).
narrative_ontology:cs_axiom_grounding('8df1e0b3-93b0-4b27-b91b-dde8069632bf', owner_discretion_default_rule, conventional).
narrative_ontology:cs_reference_frame('8df1e0b3-93b0-4b27-b91b-dde8069632bf', chattel_ontology_common_law_baseline).
narrative_ontology:cs_drift_state('8df1e0b3-93b0-4b27-b91b-dde8069632bf', contemporary_sentience_science_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8df1e0b3-93b0-4b27-b91b-dde8069632bf', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, livestock_agriculture).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, biomedical_research_sector).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, companion_animal_keepers).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_product_consumers).
narrative_ontology:constraint_vindicates(animal_status__property_reading, chattel_property_taxonomy).
narrative_ontology:constraint_vindicates(animal_status__property_reading, human_exceptionalism_legal_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and amend the statutes that keep animals inside the property taxonomy and decide which, if any, welfare limits attach to ownership. They respond to industry coalitions, constituent sentiment, and occasional ballot-measure results. Their route out of the current arrangement runs through reclassification legislation, which no majority has assembled; incremental welfare bills are the reachable margin.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legislative_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Hear the disputes this arrangement generates — ownership contests, damage claims, veterinary malpractice, custody fights — applying the object classification as settled doctrine. Petitions seeking to recast animals as legal persons are dismissed at the threshold for want of standing. Doctrine evolves at the margin, with sentience language appearing in opinions, without disturbing the classification itself.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Inspect facilities and pursue violations of the welfare statutes that constitute this arrangement's only internal limits. Mandates and budgets are set by the same legislatures that write the statutes, so enforcement intensity tracks appropriations and political attention rather than any independent assessment of need.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Own and manage the overwhelming majority of farmed animals as production assets; breeding, feeding, and slaughter scheduling proceed as ordinary asset management. The classification secures title, collateral, and insurance over the herd. Large operators can shift production across jurisdictions if any single jurisdiction tightens its rules.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, livestock_agriculture, beneficiary,
    organized, generational, arbitrage, continental).

% Hold and use animals as experimental material under licensing frameworks layered onto the ownership baseline. Physical plant, protocols, and approvals are jurisdiction-bound, so relocation in response to tightening rules is slow and expensive; the sector defends the baseline as constitutive of the research enterprise itself.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, biomedical_research_sector, beneficiary,
    institutional, generational, constrained, global).

% Keep animals as household companions under the same object classification, which governs purchase, veterinary consent, housing rules, and end-of-life decisions. Exit is easy in the narrow sense that an individual may stop keeping an animal, yet the attachment relation keeps most keepers inside the arrangement for life.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, companion_animal_keepers, beneficiary,
    moderate, biographical, mobile, local).

% Buy the outputs the arrangement keeps cheap and abundant — meat, dairy, leather, research-derived medicine — and vote occasionally on welfare ballot measures. Individual leverage is small; aggregate demand is the revenue base the owner seats optimize against.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_product_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Campaign against the classification through litigation, ballot initiative, and corporate pressure. Their suits are dismissed for want of a recognizable client; their initiatives sometimes pass and are subsequently narrowed or preempted. They operate entirely outside the formal design conversation this arrangement maintains.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_advocacy_organizations, excluded,
    organized, generational, constrained, global).

% Map the doctrine, document its internal inconsistencies such as the gap between pet protections and farm-animal exclusions, and theorize alternative ontologies. They hold no decision power; their influence runs through citation, clerkship pipelines, and the slow drift of judicial opinion language.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__property_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status__property_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable legal ontology for a world in which humans live amid dependent non-human beings: classifying animals as objects lets ownership, sale, collateral, insurance, inheritance, liability, and taxation of animals run through the same rules as other movable property, resolving an otherwise open-ended coordination problem — whose animal is it, who answers for its acts, what happens to it when its keeper dies — without requiring any theory of animal interests.
% TRANSFER_FUNCTION: Transfers complete decision authority over each animal's body, labor, reproduction, movement, and death to its owner, and transfers the entire surplus of animal-based production to owners and the markets they sell into; returns to the animals only whatever welfare statutes, enacted as human choices, happen to provide.
% ABSENT_VOICES: The animals themselves: the parties whose bodies and deaths constitute the arrangement have no seat and no standing proxy. Advocacy organizations speak publicly but are excluded from the formal conversation — courts dismiss their clients' petitions for lack of standing and legislatures receive them only as lobbyists. Neighbors bearing farm externalities likewise enter solely through ordinary tort, not through this arrangement's design process.
% DISAPPEARANCE_RATIONALE: If animals became rights-holding persons overnight, livestock agriculture, biomedical research, breeding and pet markets, veterinary practice, and meat and leather supply chains would all require immediate refounding; trillions in recognized assets would be repriced as liabilities, and the property-law machinery this reading supplies — title, collateral, insurance — would lose its entire object domain.
% FOUNDING_PROBLEM: Early legal systems needed to integrate economically indispensable living beings — draft animals, food herds — into commerce and ownership: securing lenders, resolving theft and trespass, and settling succession required that animals be ownable, alienable, and answerable-for under the same taxonomy as other valuable movables.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by comparative legal historiography tracing the chattel classification from Roman res through English common law, and by contemporary animal-law scholarship documenting both the commercial origins of the classification and its present contestation. The beneficiary industries' own efficiency attestations are noted and discounted as interested.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extraction is 0.05 because the referent is the standing arrangement read through this reading's ontology: no animal claims register, welfare-compliance costs on owners price as ordinary business expense, and dispute costs are symmetric market frictions — the residual epsilon is that friction plus thin statutory compliance burdens. Suppression is 0.35: the classification reproduces mostly through ordinary doctrine rather than heavy coercion, but the boundary requires active maintenance — courts dismissing standing petitions, legislatures preempting local ordinances, and ag-gag statutes criminalizing facility investigations supply the coercive edge. Theater is 0.30: welfare statutes partly perform reassurance (many codify industry-standard practice), while inspection systems do real, budget-limited work. Accessibility collapse is low (0.20): alternatives remain genuinely open within the reading's frame — jurisdictions differ, contractual terms vary, voluntary welfare commitments proliferate. Resistance is 0.50: sustained litigation campaigns, ballot initiatives, and scholarly critique meet the constraint continuously without prevailing inside the courtroom. The temporal series run on one shared grid (years, roughly the 1965-2025 modern animal-law era): extraction creeps up as the welfare overlay thickens; theater rises as statutes increasingly codify existing practice; suppression_requirement traces a real enforcement history — doctrinal-only at the start, an ag-gag/preemption ratchet peaking mid-interval, partial judicial rollback of ag-gag laws softening the tail. The suppression series is authored because enforcement capacity, not merely extraction, is the traced dynamic here.
 *
 * PERSPECTIVAL GAP:
 *   Within this story the seats compute alike — coordination-flavored — because the reading admits only human agents to the seat set; that homogeneity is the reading's content, not an authoring artifact. The substantive perspectival gap is BETWEEN readings: the same standing arrangement, assessed under the welfare reading's victim declarations, computes as tangled-rope-flavored, and under the abolitionist reading's as snare-flavored — those verdicts belong to the sibling files. Within-story asymmetry persists at the margins: the excluded advocacy seat experiences foreclosure rather than payment, and the governmental seats experience administration burden rather than collection, neither of which the beneficiary declarations encode.
 *
 * DIRECTIONALITY LOGIC:
 *   The four declared beneficiaries derive low directionality (near the subsidized end) from the beneficiary declarations plus generous exit options — livestock's arbitrage-grade mobility sits furthest toward the beneficiary pole. No victim declarations exist anywhere in this story, so no seat derives high directionality; the ABSENCE of a target seat is precisely what this reading asserts. The three governmental seats take power-atom canonical fallbacks, which commentary flags as approximations: they maintain the arrangement without collecting its transfer. Directionality overrides are deliberately NOT used: the override mechanism keys on power_atom alone, and this story's atom classes internally collide — 'organized' contains both a beneficiary industry and an opposed advocacy movement, and 'institutional' contains both extracting beneficiaries and non-collecting administrators — so any atom-level override would corrupt the seats the structural derivation already places correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating economically vital living beings into commerce and ownership — is still live in attenuated form: animal commerce continues and needs an ontology, so no mandatrophy declaration issues and no sunset clause applies. Nor is this a degraded-performance case: the coordination function is fully operational and actively defended by concentrated beneficiaries, which is the opposite of the cost-asymmetry profile of an inertially maintained relic. The mandatrophy question that actually matters here is cross-reading — whether the arrangement's mandate survives once animals count as parties — and that uncertainty is routed to the omega variables rather than forced into this reading's scalar metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the property_reading instantiation of the contested kernel animal_status — what structurally changes if the welfare_reading or abolitionist_reading is instantiated instead?',
    'Author and compile the sibling files: welfare_reading moves animals into the victim set and raises epsilon to moderate levels; abolitionist_reading dissolves the beneficiary structure entirely and maximizes epsilon. Cross-reading comparison of the three compiled stories locates the disagreement in the ontological assignment rather than in any empirical parameter.',
    'Classification flips across readings: this file computes rope-flavored from its own lights while the same standing arrangement computes tangled_rope- or snare-flavored under sibling victim declarations; any cross-reading verdict must compare files and never blend them into one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: one kernel, three readings, three distinct constraints with distinct epsilon and victim structures.').

omega_variable(
    boundary_construction_vs_discovery,
    'Is the human/thing legal boundary a discovered feature of moral reality, as this reading''s adherents assert, or a constructed convention concentrating usable assets in identifiable beneficiary classes?',
    'Test whether the boundary tracks independently defensible criteria or beneficiary interest: if protections expand exactly where commercial exposure is lowest (companion animals) and stall where it is highest (farmed animals), the boundary is tracking interest, not criterion.',
    'If constructed-and-benefiting, false-summit dynamics become relevant despite the rope claim — the classification behaves as a naturalized convention serving named seats, and reclassification pressure should escalate with each exposed inconsistency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_construction_vs_discovery, conceptual, 'Whether the object/person boundary is discovered moral structure or beneficiary-serving construction.').

omega_variable(
    sentience_evidence_threshold,
    'At what point does accumulated sentience and cognition evidence convert welfare statutes from voluntary overlays into de facto interest-recognition that this reading can no longer absorb?',
    'Track legislative responses to sentience findings such as cephalopod and decapod recognition, and judicial handling of cognition evidence in custody and standing disputes; the marker is statutes conferring interest-like protections no longer framed through ownership.',
    'Past the threshold, this reading''s epsilon stops being assessable in its own lights: the reading collapses toward the welfare reading''s constraint and this file''s empty victim set must be reopened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_evidence_threshold, empirical, 'Empirical trajectory at which the welfare overlay becomes interest-recognition.').

omega_variable(
    excluded_seat_access_route,
    'Will animal advocacy organizations acquire a formal access route — proxy guardianship, statutory standing — that converts the excluded seat into a seated party?',
    'Track habeas corpus and guardianship litigation outcomes plus statutes granting advocate participation rights; the nonhuman-rights litigation line and analogous statutory ombudsman models are the leading indicators.',
    'If access opens, the absent-voices condition ends, the consensus-provenance check gains a dissenting insider, and the suppression component carried by threshold-standing doctrine becomes contestable inside the courtroom rather than only outside it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_seat_access_route, empirical, 'Whether the excluded advocacy seat acquires formal access to the arrangement''s design conversation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__property_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(anim_tr_t0, observed).
narrative_ontology:measurement(anim_tr_t10, animal_status__property_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(anim_tr_t10, observed).
narrative_ontology:measurement(anim_tr_t20, animal_status__property_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(anim_tr_t20, observed).
narrative_ontology:measurement(anim_tr_t30, animal_status__property_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(anim_tr_t30, observed).
narrative_ontology:measurement(anim_tr_t40, animal_status__property_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(anim_tr_t40, observed).
narrative_ontology:measurement(anim_tr_t50, animal_status__property_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(anim_tr_t50, observed).
narrative_ontology:measurement(anim_tr_t60, animal_status__property_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(anim_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__property_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(anim_be_t0, observed).
narrative_ontology:measurement(anim_be_t10, animal_status__property_reading, base_extractiveness, 10, 0.03).
narrative_ontology:measurement_basis(anim_be_t10, observed).
narrative_ontology:measurement(anim_be_t20, animal_status__property_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement_basis(anim_be_t20, observed).
narrative_ontology:measurement(anim_be_t30, animal_status__property_reading, base_extractiveness, 30, 0.04).
narrative_ontology:measurement_basis(anim_be_t30, observed).
narrative_ontology:measurement(anim_be_t40, animal_status__property_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement_basis(anim_be_t40, observed).
narrative_ontology:measurement(anim_be_t50, animal_status__property_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement_basis(anim_be_t50, observed).
narrative_ontology:measurement(anim_be_t60, animal_status__property_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement_basis(anim_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__property_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(anim_su_t0, observed).
narrative_ontology:measurement(anim_su_t10, animal_status__property_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(anim_su_t10, observed).
narrative_ontology:measurement(anim_su_t20, animal_status__property_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(anim_su_t20, observed).
narrative_ontology:measurement(anim_su_t30, animal_status__property_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement_basis(anim_su_t30, observed).
narrative_ontology:measurement(anim_su_t40, animal_status__property_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(anim_su_t40, observed).
narrative_ontology:measurement(anim_su_t50, animal_status__property_reading, suppression_requirement, 50, 0.37).
narrative_ontology:measurement_basis(anim_su_t50, observed).
narrative_ontology:measurement(anim_su_t60, animal_status__property_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement_basis(anim_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__property_reading, animal_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the legal status of animals' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints corresponding to the three readings of the animal_status kernel: this file (property_reading, epsilon 0.05, no victim set), animal_status__welfare_reading (interests constrain use; moderate epsilon; animals in the victim set), and animal_status__abolitionist_reading (rights-holders; maximal epsilon; instrumental-use beneficiaries dissolved). Each story carries its own claimed type, metrics, and stakeholders; they are linked pairwise through network.affects_constraints. Upstream/downstream structure: the property classification is the historical baseline from which the welfare overlay grew (influences relation) and against which the abolitionist reading stands in direct logical contradiction (forecloses relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
