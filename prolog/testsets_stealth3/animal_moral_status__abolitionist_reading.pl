% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Animal Use Under Property Status (Abolitionist Reading: Property Status Itself Is the Violation)
 *   domain: applied_ethics/legal_philosophy/animal_studies
 *
 * SUMMARY:
 *   This story instantiates the abolitionist_reading of the
 *   animal_moral_status kernel (see commentary.kernel_context and
 *   cs_structure). The arrangement under description is the standing one:
 *   animals held as legal property and used for food, fiber, labor, research,
 *   and entertainment. From this reading's seat, property status itself is
 *   the violation and all use, however humane, perpetuates victimization; per
 *   the epsilon-referent rule, epsilon is authored for the standing
 *   instrumental-use arrangement as this reading assesses it (near-total: the
 *   arrangement takes the animals' liberty, bodies, reproductive lives, and
 *   lives, and no compensating flow runs back to them), never for the
 *   rights-respecting arrangement the reading endorses. The kernel decomposes
 *   into three readings, each a separate constraint file with its own
 *   epsilon, victim set, and type, linked by network.affects_constraints: the
 *   property_reading (no independent standing; the operative legal baseline),
 *   the welfare_reading (cruelty is the violation, use is permissible; the
 *   operative regulatory overlay), and this one (use itself is the
 *   violation). Claim and metrics are independent authored facts:
 *   claimed_type snare is this reading's structural verdict — the humane-use
 *   coordination story is cover, and persistence runs on coercion and
 *   foreclosed exits — while the metrics are authored as this reading
 *   assesses them, without reference to any predicted engine output. Interval
 *   framing assumption: T=0 to T=30 approximates 1995 to 2025, the era of the
 *   modern abolitionist critique and the welfare-certification boom. The
 *   stakeholder set is a global composite; jurisdictions differ in welfare
 *   detail but the property baseline is universal.
 *
 * KEY AGENTS:
 *   - animals_under_human_dominion: primary target (powerless/trapped) — bears the entire cost; legally things, structurally voiceless, no exit exists from where they stand
 *   - animal_use_industries: agenda-setter and capture seat (powerful/arbitrage) — operates the use-relationship and collects its direct economic value
 *   - animal_product_consumers: beneficiary (moderate/mobile) — receives the products; exit is available and mostly unexercised
 *   - animal_research_institutions: beneficiary (institutional/constrained) — collects publications, patents, and training pipelines built on animal models
 *   - animal_law_systems: agenda-setter (institutional/constrained) — constitutes and enforces property status; cannot exit its own constitutive act
 *   - welfare_regulatory_agencies: agenda-setter and beneficiary (institutional/constrained) — administers the humane overlay that legitimates the baseline
 *   - welfare_advocacy_organizations: beneficiary (organized/identity_locked) — collects support for welfare work; identity-fused with the welfare frame
 *   - animal_rights_abolitionists: analytical observer (organized/analytical) — sees the full structure; excluded from the rule-setting conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.9).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.85).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Use Under Property Status (Abolitionist Reading: Property Status Itself Is the Violation)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/legal_philosophy/animal_studies").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, 'b0281b2c-fbf8-4571-b67d-eb7f6986e1d5').
narrative_ontology:cs_kernel_codification('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', distributed).
narrative_ontology:cs_authority_grounding('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', expertise).
narrative_ontology:cs_interpretation_layer_present('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5').
narrative_ontology:cs_reading_relation('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', foundational, sentience_confers_inviolable_rights).
narrative_ontology:cs_axiom_status(sentience_confers_inviolable_rights, holdable).
narrative_ontology:cs_axiom_grounding('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', sentience_confers_inviolable_rights, deontological).
narrative_ontology:cs_axiom('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', foundational, property_status_itself_is_the_violation).
narrative_ontology:cs_axiom_status(property_status_itself_is_the_violation, holdable).
narrative_ontology:cs_axiom_grounding('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', property_status_itself_is_the_violation, deontological).
narrative_ontology:cs_axiom('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', secondary, welfare_reform_perpetuates_victimization).
narrative_ontology:cs_axiom_status(welfare_reform_perpetuates_victimization, holdable).
narrative_ontology:cs_axiom_grounding('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', welfare_reform_perpetuates_victimization, instrumental).
narrative_ontology:cs_reference_frame('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', animals_as_rights_bearing_individuals).
narrative_ontology:cs_drift_state('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', contemporary_operative_law, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b0281b2c-fbf8-4571-b67d-eb7f6986e1d5', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_product_consumers).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_research_institutions).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, welfare_regulatory_agencies).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, welfare_advocacy_organizations).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, animals_under_human_dominion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bred, confined, transported, and killed under human control across farming, research, entertainment, and companionship. Legally things rather than persons: every protection runs through an owner or a regulator, and no animal holds standing in any court or legislature. The interests at stake — continued life, bodily liberty, freedom from confinement — are the interests the arrangement exists to override. Exit does not exist from where they stand: the only way out of the use-relationship would be recognition as a subject of rights, and the property frame is precisely what withholds that recognition. They would object to every part of this arrangement if they could reach the conversation; they cannot, and the arrangement is what keeps them unable to.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animals_under_human_dominion, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, animals_under_human_dominion, excluded).

% Operate the use-relationship directly — breeding, confinement, slaughter, research protocols, performance — and shape its rules through lobbying, model legislation, and trade pressure. Collect the direct economic value of animal bodies, labor, and reproductive capacity; consumers pay into this seat and the law secures its title. When welfare rules tighten in one jurisdiction, production shifts to laxer ones; the realistic exit is jurisdictional arbitrage, not exit from animal use, which is the business itself.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_use_industries, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, animal_use_industries, beneficiary).

% Receive the arrangement's products — meat, dairy, eggs, leather, research-derived medicine — at prices that do not carry the costs borne by the animals. Exit is materially available: plant-based alternatives exist, are increasingly accessible, and a minority takes them. The majority does not, for a mix of reasons this story treats as partly structural (price, access, habit, culture) and partly internalized (the sense that animal products are necessary or natural).
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, global).

% Use animals as models under review committees that administer the humane-use standards. Collect publications, patents, funding pipelines, and training programs built on animal models. Non-animal methods exist and are growing but remain incomplete for many programs, so leaving the models is partial, slow, and expensive; the institution's sunk infrastructure and career structures point the other way.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Constitute animals as legal property in every jurisdiction: ownership, sale, inheritance, liability, and the boundary of permissible use are all defined here, and the arrangement is enforced through ordinary police power — including statutes that criminalize unauthorized documentation of farm conditions and enhance penalties for interference with facilities. The law cannot exit its own constitutive act; property status is its current answer to the moral-status question, and revising it requires the law to act against the interests of everything that depends on the current answer.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_law_systems, agenda_setter,
    institutional, generational, constrained, national).

% Administer the humane-treatment overlay — housing, transport, slaughter, enrichment standards — on top of the property baseline. Their standards bind how owners treat animals but never question ownership itself; the remit stops at the property line the statutes draw. They derive budgets, mandate, and institutional relevance from administering the overlay, which gives them a standing interest in the overlay's continuation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, welfare_regulatory_agencies, beneficiary).

% Campaign for improved treatment within use systems — larger enclosures, slower slaughter, enriched environments — negotiating with industry and regulators and collecting donations, membership, and institutional support for that work. Their organizational identities are built around the welfare mission; moving to a position that use itself must end would dissolve the identity the organizations are made of, so the welfare frame is the one thing they cannot give up even where their rhetoric drifts toward it.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, global).

% Scholars, lawyers, and activists who hold that property status itself is the violation and that every use perpetuates it. They litigate personhood petitions, document conditions, and make the structural case in public. They hold no position inside the use-relationship and no seat in its rule-setting: legislatures and agencies entertain welfare reforms but not abolition, documentation is criminalized in several jurisdictions, and personhood petitions are rejected as a category error. Their seat is analytical — they see the whole structure and stand outside it.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_rights_abolitionists, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__abolitionist_reading, animal_use_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The property framework solves a real coordination problem, stated without evaluation: it provides a single legal operating system for human dealings with animals — ownership, transfer, breeding, liability, veterinary duty, transport, and adjudication of competing human claims — applied uniformly across billions of animals and centuries of practice. What it coordinates is human use; the animals are its objects, not its parties.
% TRANSFER_FUNCTION: Moves the bodies, labor, reproductive capacity, and lives of animals into human use as food, fiber, draft power, research data, and spectacle, and moves the resulting economic value from consumers to the industries that process and sell it. The animals transfer everything and receive nothing; no compensating flow to them exists, because compensation presupposes a subject and the arrangement's legal form denies them that status.
% ABSENT_VOICES: The animals themselves — the only parties whose interests are totally and non-optionally at stake — hold no seat anywhere in the arrangement: no standing, no representation, no franchise; they enter the conversation only as objects of it (stakeholder animals_under_human_dominion carries the excluded secondary role). Their interests reach the table exclusively through human advocates, who speak at a structural discount and are themselves partly outside the rule-setting conversation — documentation of use-conditions is criminalized in several jurisdictions. The seat where the strongest objection would originate is the seat the arrangement is built to keep empty.
% DISAPPEARANCE_RATIONALE: Roughly a third of global agricultural output, the majority of biomedical research models, entire doctrines of ownership and liability, and the dietary practice of most of humanity are organized around this arrangement. If property status and the use-relationship vanished overnight, food systems, research pipelines, land use, trade, and criminal and civil law would all reorganize — the largest single restructuring of the human economy on record. The world would not merely notice the disappearance; it is built out of it.
% FOUNDING_PROBLEM: The arrangement was built to solve the problem of securing reliable human access to animal bodies and labor — food security, draft power, materials, and later scientific models — at scale. Constituting animals as property was the legal technology that made that access administrable: ownable, transferable, inheritable, and lienable.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological and historical scholarship on domestication attests the founding problem and its original function from outside the benefiting parties. Whether the problem remains live is disputed with no neutral arbiter: the necessity defense (nutrition, research necessity, tradition) is maintained almost exclusively by the benefiting parties and their funded research, while the dissolution case (plant-based adequacy, non-animal methods) is advanced by advocates and independent researchers whom the beneficiaries dismiss as interested. No party outside the dispute attests that the founding problem still exists as originally stated.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.9, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon 0.90: from this seat the arrangement takes everything the reading holds inviolable — continued existence, bodily liberty, reproductive autonomy — from every animal under dominion, and returns nothing that runs to them. Suppression 0.85 (raw, unscaled by power or scope): persistence runs on total physical control of the victims plus a hardening legal apparatus against human challengers — ag-gag statutes, terrorism enhancements, facility-interference prosecutions; at the animals' end suppression is wholly structural, at the human end it is mixed structural and internalized (see omega consumer_exit_barriers_internalization). Theater_ratio 0.62: the welfare-certification layer has grown faster than any measured suffering reduction, and this reading assesses its dominant function as manufacturing the moral license that keeps use in place. Accessibility_collapse 0.65: the relevant alternative — a rights-respecting relation — is foreclosed in every jurisdiction's law even where behavioral exit (plant-based consumption) is materially available; alternatives are visible but legally blocked at exactly the level the arrangement occupies. Resistance 0.55: sustained and growing (personhood litigation, open rescue, ballot measures, shifting public opinion) but met with enforcement hardening rather than concession. The usual coalition remedy for powerless victims — many small sufferers combining into a bargaining force — is unavailable by design: the victims cannot communicate, organize, own, or sue; their interests reach the conversation only through human proxies, and the proxy channel is itself part of what the enforcement apparatus polices. The three tracked series share one six-point grid, every metric authored at every point; suppression_requirement is tracked because enforcement-capacity change (the ag-gag buildout and prosecutorial intensification) is a traced dynamic of this story, not a static backdrop. Trajectories are monotonic, not cyclical: extraction creeps up with the global scale of use, theater rises with certification growth, enforcement hardens against resistance. Boltzmann note: coordination_type resource_allocation is declared because the property machinery does perform a real allocation function (title, transfer, liability), but boltzmann_floor_override is set to 0.05 because the machinery is generic property law — its application to animals adds no inherent coordination cost that could excuse the measured extraction as coordination overhead; from this seat, the coordination and the taking are the same act.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the animal_law_systems and animal_use_industries seats the arrangement is a functioning legal-economic order — coordination, title, liability, food security — and those seats should compute a low-extraction, coordination-forward type. From the animals_under_human_dominion seat the same structure computes as total extraction with no exit. Consumers sit between: genuine benefit, available exit, unexercised. Same-level actors diverge on constraint-specific factors, not global standing: industries and research institutions are both institutional beneficiaries, but the industries hold jurisdictional arbitrage while the institutions are sunk into their models; consumers and abolitionists are both unaligned humans, but one holds mobile exit it does not take and the other an analytical seat outside the relationship entirely. The welfare seats experience the arrangement as a reformable machine they are improving; this reading's claim is precisely that this experience is the cover story operating, and the identity_locked exit of the advocacy organizations — their institutional selves are made of the welfare mission — is the mechanism by which the welfare frame reproduces itself. The engine computes per-seat classifications from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Victim declaration: animals_under_human_dominion — trapped, powerless — derives directionality at the full-target end, and there is no exit modulation because the only exit (recognition as a rights-bearing subject) is what the arrangement's legal form forecloses; trapped-or-worse-than-trapped. Beneficiary declarations map to real receipt: the industries collect the direct economic value (near the beneficiary end, reinforced by arbitrage-grade jurisdictional exit); consumers receive the products with mobile exit (nearest the beneficiary end); research institutions collect publications and pipelines under constrained exit; the welfare apparatus collects mandate, budgets, and donations. This reading's distinctive structural claim is that the welfare seats are beneficiaries of the cover story — which is why they are declared beneficiaries here despite presenting as reformers. The law systems administer without collecting the gains and are left to the canonical fallback rather than declared beneficiaries, which matches their seat: constitutive enforcement, not receipt. No directionality_overrides are used: the role-plus-exit derivation already distinguishes the institutional seats (law systems administer without collecting; agencies and advocacy organizations collect through the overlay), so the structural data carries the differentiation an override would otherwise force.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is anti-mandatrophy in both directions. First, it blocks the rope-mislabeling: the welfare framework presents itself as pure coordination — minimizing suffering within use — and a seat inside that frame would compute a rope or scaffold; this reading's snare claim asserts the coordination story is cover: the arrangement coordinates use, not welfare, and its persistence runs on coercion and foreclosed exits rather than on participant benefit. Second, it blocks the false-mountain mislabeling: the property_reading presents the arrangement as a natural baseline — animals have always been property, human use is a fact of nature — and the beneficiaries declared here are exactly what defeats that naturality: an arrangement with beneficiaries is maintained by them, and what is maintained is not a mountain. The R5 genealogy records the founding problem (organizing human access to animal bodies and labor) as contested: the benefiting parties maintain it is live (necessity of food, research, tradition), the dissolution case says it is dead (plant-based adequacy, non-animal methods), and no party outside the dispute attests either way. The mismatch consumer sees status=contested with verdict=world_rearranges — no zombie flag, but the genealogy is flagged as the live battlefield rather than settled history, which is the honest state of this constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_animal_moral_status_kernel,
    'This constraint is the abolitionist_reading of the animal_moral_status kernel — what would the sibling readings (animal_moral_status__property_reading, animal_moral_status__welfare_reading) change structurally if instantiated instead, and where exactly is the disagreement located?',
    'The siblings are separate constraint files with their own epsilon, victim sets, and types; the disagreement is located in the status assignment and in what follows from sentience: this reading holds property status itself is the violation, the property_reading holds it is a legitimate background condition (victim set empties by definitional fiat), and the welfare_reading holds only cruelty above a baseline violates (victim set shrinks to treatment, not use). Cross-reading comparison happens between files, never inside this one.',
    'Under the property_reading the victim set is empty and epsilon collapses toward the coordination-cost floor; under the welfare_reading epsilon drops to moderate and the theater layer reads as genuine function. This file''s high epsilon holds only within the abolitionist reading''s frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_animal_moral_status_kernel, conceptual, 'Committer structure: one reading of a contested kernel; siblings instantiate structurally different constraints from the same commitment.').

omega_variable(
    contingent_vs_structural_property_status,
    'Is the property status of animals a contingent legal construct that could be replaced by a personhood framework without the use-relationship reconstituting itself in another form, or is dominion structural to any use-relationship — making the extraction a property of use itself rather than of property law?',
    'Track jurisdictions that grant limited personhood or rights (habeas petitions, rights-of-nature analogues) and observe whether use persists through guardianship, stewardship, or ownership-in-function forms.',
    'If contingent, the arrangement is legally reformable and abolition is reachable by ordinary legal change; if structural, extraction reconstitutes under any legal form that leaves use intact, and the verdict describes the use-relationship itself rather than the property frame that currently carries it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_vs_structural_property_status, conceptual, 'Whether property status is the load-bearing wall of the arrangement or one legal form of a deeper dominion structure.').

omega_variable(
    welfare_layer_function_ambiguity,
    'Is the welfare overlay''s dominant function genuine suffering-reduction or legitimation of continued use — how much of the measured theater_ratio is irreducible?',
    'Longitudinal welfare-outcome studies set against legitimation indicators: certification growth versus per-animal welfare gains, moral-licensing effects on consumption, and whether certified and uncertified systems converge in slaughter volumes.',
    'If welfare gains are substantial and non-theatrical, theater_ratio falls and the arrangement reads as hybrid coordination-plus-extraction; if legitimation dominates, the cover-story reading hardens and welfare reform is confirmed as the arrangement''s moral-license machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_layer_function_ambiguity, empirical, 'Whether the welfare apparatus reduces suffering or manufactures the license that keeps use in place.').

omega_variable(
    consumer_exit_barriers_internalization,
    'Why do most consumers not exit the use-relationship when exit is materially available — is the binding barrier structural (price, access, habit, culture) or internalized (the sense that animal products are necessary or natural)?',
    'Post-exit trajectory studies of people who leave animal use: if perceived necessity and identity threat persist after structural barriers are removed, the barrier is partly internalized; if they dissolve, it was structural.',
    'If internalized, the arrangement''s effective hold exceeds what structural measures capture — it travels inside the beneficiary and would persist after legal reform, meaning abolition of property status alone would not end the use-relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_exit_barriers_internalization, empirical, 'Structural versus internalized barriers on the human side of the arrangement.').

omega_variable(
    victim_interest_attribution,
    'How rich are the interests attributable to the victims — does the epsilon assessment''s foundation (interests in continued life and bodily liberty) hold across all animals under dominion and equally across taxa?',
    'Converging cognitive-ethology and welfare-science evidence on sentience, preference, and life-value across taxa; the attribution strengthens or weakens as the cross-taxa evidence base broadens.',
    'If the attributable interests are thinner or more uneven than this reading assumes, epsilon falls toward the welfare_reading''s assessment; if richer, the high value holds or rises and the victim set is, if anything, understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_interest_attribution, empirical, 'Whether the victims'' attributed interests bear the weight the epsilon assessment places on them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(anim_tr_t6, animal_moral_status__abolitionist_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement(anim_tr_t12, animal_moral_status__abolitionist_reading, theater_ratio, 12, 0.54).
narrative_ontology:measurement(anim_tr_t18, animal_moral_status__abolitionist_reading, theater_ratio, 18, 0.58).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__abolitionist_reading, theater_ratio, 24, 0.6).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__abolitionist_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.84).
narrative_ontology:measurement(anim_be_t6, animal_moral_status__abolitionist_reading, base_extractiveness, 6, 0.85).
narrative_ontology:measurement(anim_be_t12, animal_moral_status__abolitionist_reading, base_extractiveness, 12, 0.86).
narrative_ontology:measurement(anim_be_t18, animal_moral_status__abolitionist_reading, base_extractiveness, 18, 0.88).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__abolitionist_reading, base_extractiveness, 24, 0.89).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(anim_su_t6, animal_moral_status__abolitionist_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(anim_su_t12, animal_moral_status__abolitionist_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(anim_su_t18, animal_moral_status__abolitionist_reading, suppression_requirement, 18, 0.77).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__abolitionist_reading, suppression_requirement, 24, 0.81).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__abolitionist_reading, 0.05).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__abolitionist_reading, animal_moral_status__welfare_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'the moral status of animals' covers three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story constraint family sharing the animal_moral_status kernel. The epsilon values differ because the violation-scope differs by reading over the same standing arrangement: the property_reading finds no violation (animals lack standing by definition; epsilon near the coordination-cost floor), the welfare_reading finds violation only above a cruelty baseline (epsilon moderate), and this abolitionist_reading finds the property status itself to be the total violation (epsilon high). This story is the contesting reading: it assesses the arrangement the other two readings administer or accept. Upstream/downstream structure: the property_reading is the legal baseline the other readings litigate against, and the welfare_reading is the regulatory overlay this reading characterizes as the cover story; both are linked here so purity degradation in any member propagates across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
