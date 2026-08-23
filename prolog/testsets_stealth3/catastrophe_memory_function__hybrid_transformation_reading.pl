% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Passover Seder as Hybrid Mourning-Survival Ritual Structure
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   The standing arrangement under contest is the annual household Passover
 *   seder as practiced across the diaspora: a fixed-order evening in which
 *   bitter herbs and salt water carry the memory of servitude and loss, while
 *   the scripted questioning of children, the duty of retelling, and the
 *   deliberately portable home-based format rehearse the practices by which a
 *   stateless community has repeatedly survived institutional destruction.
 *   This file instantiates the hybrid_transformation_reading of the kernel
 *   catastrophe_memory_function: the claim is that both registers are encoded
 *   in one structure — the mourning elements preserve loss-memory (D1/D4)
 *   while the performative architecture transmits adaptive capacity (D5) —
 *   and the epsilon authored here (0.28) is assessed over that hybrid
 *   referent by this reading's own lights, never over the arrangement either
 *   sibling reading would prefer. The sibling readings
 *   (catastrophe_memory_function__mourning_practice_reading,
 *   catastrophe_memory_function__survival_competence_reading) are separate
 *   constraints in separate files, linked through the network edge; the
 *   contest between them is recorded in the committer omega, not adjudicated
 *   here. The claimed type (rope) and the metrics below are independent
 *   authored facts: the metrics describe an arrangement whose costs are real
 *   but borne by net beneficiaries under light social enforcement.
 *
 * KEY AGENTS:
 *   - - diaspora_jewish_households: primary beneficiary seat (organized/global, identity_locked) — conducts the practice, receives continuity, bears its weekly costs
 *   - - jewish_children: designated transmission recipients (powerless/global, trapped) — scripted questioners, obligated before consent
 *   - - rabbinic_transmission_authorities: agenda-setter (institutional/global, identity_locked) — maintains requirements and texts, collects standing from the practice's centrality
 *   - - seder_preparation_laborers: cost-bearing seat (moderate/global, constrained) — carries the preparation burden, shares the meaning
 *   - - secular_and_intermarried_members: marginal participants (moderate/global, constrained) — partial uptake under family pull
 *   - - lapsed_practice_descendants: excluded voice (moderate/global, mobile) — objects from outside the gathered households
 *   - - holocaust_memory_institutional_complex: complementary institutional observer (institutional/continental, analytical)
 *   - - ritual_studies_analysts: analytical observer (analytical/global, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Passover Seder as Hybrid Mourning-Survival Ritual Structure").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).
narrative_ontology:has_sunset_clause(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, 'e93d78ee-7444-47d7-a406-4e94feae3858').
narrative_ontology:cs_kernel_codification('e93d78ee-7444-47d7-a406-4e94feae3858', fixed_text).
narrative_ontology:cs_authority_grounding('e93d78ee-7444-47d7-a406-4e94feae3858', lineage).
narrative_ontology:cs_interpretation_layer_present('e93d78ee-7444-47d7-a406-4e94feae3858').
narrative_ontology:cs_reading_relation('e93d78ee-7444-47d7-a406-4e94feae3858', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('e93d78ee-7444-47d7-a406-4e94feae3858', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('e93d78ee-7444-47d7-a406-4e94feae3858', foundational, mourning_and_survival_functions_are_structurally_fused).
narrative_ontology:cs_axiom_status(mourning_and_survival_functions_are_structurally_fused, holdable).
narrative_ontology:cs_axiom_grounding('e93d78ee-7444-47d7-a406-4e94feae3858', mourning_and_survival_functions_are_structurally_fused, empirically_contingent).
narrative_ontology:cs_axiom('e93d78ee-7444-47d7-a406-4e94feae3858', foundational, commemorative_performance_transmits_adaptive_capacity).
narrative_ontology:cs_axiom_status(commemorative_performance_transmits_adaptive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e93d78ee-7444-47d7-a406-4e94feae3858', commemorative_performance_transmits_adaptive_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('e93d78ee-7444-47d7-a406-4e94feae3858', dual_function_commemorative_survival_ritual).
narrative_ontology:cs_drift_state('e93d78ee-7444-47d7-a406-4e94feae3858', contemporary_postwar_diaspora, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e93d78ee-7444-47d7-a406-4e94feae3858', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, jewish_children).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_transmission_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, seder_preparation_laborers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, secular_and_intermarried_members).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_households).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, jewish_children).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, seder_preparation_laborers).
narrative_ontology:constraint_victim(catastrophe_memory_function__hybrid_transformation_reading, secular_and_intermarried_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, embodied_reenactment_outlasts_text_alone).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, decentralized_household_ritual_resists_institutional_destruction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each spring the household gathers for the ordered evening: preparing foods, setting the table with its symbolic items, retelling the Exodus narrative, hosting guests and relatives. What flows to it is continuity of family identity, a fixed calendar anchor, and an occasion that gathers scattered members who assemble for little else. What flows from it is days of preparation, real expense, and a week of household discipline around the holiday's food rules. Leaving the practice — through intermarriage or secular drift — is possible and increasingly common, but many experience it as severing the line to ancestors who kept the same table; households rarely take that step lightly.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_households, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, diaspora_jewish_households, payer).

% Children are the evening's designated audience: the liturgy scripts their questions ('why is this night different'), assigns the adults a duty to answer at the child's level, and hands them songs and a story that arrive years before they could evaluate them. They receive belonging, narrative, and a participatory role no other family occasion grants them. They also owe attendance and performance long before they could choose it, and their boredom or rebellion is managed by the adults rather than negotiated.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, jewish_children, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, jewish_children, payer).

% Rabbis, halakhic decisors, and educators maintain what the evening requires: which foods, which texts, which questions, in what order. They compile and endorse the printed guides, rule on proposed additions, and teach the practice to each generation. Their standing rests on the unbroken chain they represent; stepping outside the received frame would dissolve the very authority they exercise. Deference and institutional continuity flow to them from the practice's centrality, and they spend real effort keeping the sequence intact across wildly different host cultures.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, rabbinic_transmission_authorities, agenda_setter,
    institutional, civilizational, identity_locked, global).

% In many households a subset of members — historically disproportionately women — carry the shopping, cooking, cleaning, and table-setting the evening demands, often beginning days ahead, while the liturgy's speaking parts traditionally went to others at the table. They share fully in the meaning made that night and increasingly share its leadership too, but the labor allocation has been the slowest element to change. Declining the labor while keeping a place at the table draws family friction; declining the evening entirely costs more than the labor does.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, seder_preparation_laborers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, seder_preparation_laborers, beneficiary).

% Members who keep none of the surrounding commandments still show up to this evening more than to any other observance — pulled by family, ambivalent about the theology, sometimes quietly editing the printed guide to fit. They absorb gentle pressure to host, attend, and affirm-adjacent, and they receive in return a usable link to ancestry and an annual seat in the family. Full withdrawal would cost them standing with people they love; full conformity costs them intellectual honesty. Most live permanently in the edit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, secular_and_intermarried_members, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, secular_and_intermarried_members, beneficiary).

% Adults raised in the practice who stopped attending. They are discussed at the table more than they are heard from; their absence registers as loss in some households and as betrayal in others. From outside, they object that the annual gathering polices a belonging they no longer claim, and that nostalgia for their attendance substitutes for engaging their actual reasons for leaving. They have already exited and bear little further cost, which is precisely why their testimony carries no leverage inside the room.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, lapsed_practice_descendants, excluded,
    moderate, biographical, mobile, global).

% Museums, memorials, archives, and schools that curate catastrophe memory through testimony, exhibits, and curricula. They track how families transmit memory informally and build programs alongside the household practice; their assessments of what transmission methods work draw on the domestic evening as a comparison case, and their programming competes with it for the same attention without governing it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, holocaust_memory_institutional_complex, observer,
    institutional, generational, analytical, continental).

% Scholars of ritual, memory, and religion who compare commemorative systems across cultures, code what the seder preserves and what it transmits, and publish analyses the practitioners may never read. Their seat sees the whole structure — both registers, the enforcement history, the exit patterns — without keeping the practice or bearing its costs.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__hybrid_transformation_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__hybrid_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reproduces catastrophic memory and group identity across generations under dispersion: one fixed evening per year in every household, with scripted child questions and obligated adult answers, guarantees the founding catastrophe is retold and recruits each generation into becoming its next tellers — a transmission job no school, book, or voluntary association reliably performs alone.
% TRANSFER_FUNCTION: Moves hours of labor, money, and attention from household members (concentrated on preparers and hosts) into a staged act of collective remembering; moves narrative, song, and survival-relevant habits from elders and text to children; moves visible standing to those who lead the table and administer the sequence.
% ABSENT_VOICES: Lapsed descendants who stopped attending — their reasons for leaving are discussed in their absence more than heard. Women whose speaking parts were historically limited while their kitchen labor carried the evening. Survivors' descendants for whom mandated annual mourning reopened grief rather than containing it. They sit outside the gathered households, or silent at their edges.
% DISAPPEARANCE_RATIONALE: Without the annual household gathering, transmission would fall to schools, museums, and voluntary reading — channels that reach fewer households, recruit no children as questioners, and lack the domestic embedding that carried the practice through expulsions, ghettoes, camps, and assimilationist pressure. Identity continuity would thin measurably within two to three generations; the community's most robust memory technology, and the rehearsal structure that made it destruction-resistant, would simply be gone.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE ended sacrifice-based observance of the festival, the rabbinic movement faced a community stripped of its central institution and progressively of its territory: how does a defeated, dispersed people keep the covenant-memory of Egypt alive, and keep the practical capacity to endure the next catastrophe, without priests, temple, or state?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Mishnah Pesachim and early rabbinic sources document the deliberate redesign of the festival for landless, priestless observance; historians of religion writing from outside the tradition treat the post-70 household seder as a paradigmatic case of post-destruction ritual substitution; and Holocaust-era testimonies and artifacts — seders conducted in ghettos and camps — attest the practice functioning under extremity, confirming that its transmission and endurance functions are real rather than self-asserted by the community that benefits from them.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.28: the arrangement costs its participants days of preparation, a week of dietary discipline, real money, and the emotional weight of mandated annual mourning, but those costs are paid by a population that receives identity continuity, calendar anchoring, and intergenerational connection in return — net extraction is modest, not negligible. Suppression is authored at 0.22 and is a raw structural property, unscaled by power or scope: persistence runs primarily on value and identity rather than sanction; enforcement is familial and communal suasion, exit through secularization exists and is exercised, and the residual coercive edge falls on those who cannot easily refuse (children, spouses, marginal members). Theater is 0.18: the ritual demonstrably transmits — it is the most widely observed practice in the population, including among the otherwise unobservant — with rote performance concentrated at the margins. Accessibility_collapse is 0.45: alternatives exist (museum commemoration, national memorial days, family storytelling, textual study) but none fuses mourning and transmission into one portable annual act embedded in the household, so alternatives persist as partial substitutes rather than replacements. Resistance is 0.22: feminist labor critique, secular editing of the text, and children's restiveness are sustained but not destabilizing. The temporal series run on one shared eight-point grid (every tracked metric authored at every point, per the alignment rule). The trajectory is a long wave, not a cycle: extraction and enforcement rose together through the medieval kehillah era (compelled observance under confined conditions, peaking near 1500), collapsed with emancipation's dissolution of communal coercive powers around 1800, and stabilized at low-modern levels; theater dipped sharply at 1945 because crisis re-functionalized the practice — seders held in ghettos and camps were maximally functional performances — before drifting mildly upward again as prosperity enabled rote observance. There is no oscillation mechanism to document; the 1945 inflection is an external-shock effect, not intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from identical structural data. From the household seat the evening is a gift that costs: continuity received, labor given, exit experienced as ancestral severance. From the preparation-laborer seat the same evening concentrates its costs unevenly — days of work historically paired with liturgical silence — while delivering the same meaning; that seat sits nearest the target end of any in the story. From the child's seat the practice is enchantment braided with compulsion: the liturgy scripts their questions, which is both recruitment and conscription. From the rabbinic-authority seat it is stewardship of an unbroken chain; the authority's identity is fused with the function it administers, so revision of the frame reads as self-dissolution. From the lapsed-descendant seat the annual gathering reads as a belonging-police operating at one remove. The engine computes these divergences from power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared at the structural layer: diaspora_jewish_households, jewish_children, and rabbinic_transmission_authorities each derive low directionality (subsidized seats). No victims are declared because no group bears net uncompensated harm under this reading's referent — the cost-bearing seats (seder_preparation_laborers, secular_and_intermarried_members) hold payer roles with beneficiary secondary positions, placing them mid-range rather than at the target pole; the gendered_labor_attribution omega governs whether the laborer seat should be weighted further toward target. Identity-locked exit on the households and authorities does not amplify extraction here because those seats are beneficiaries: lock-in stabilizes their subsidy of the structure rather than deepening their exposure. Trapped children combine powerlessness with beneficiary position — near-symmetric with a benefit tilt, since the obligation precedes consent but the transmission lands on them. No directionality overrides are authored: the derivation from beneficiary declarations, power atoms, and exit options produces the seat relationships described, and the two genuinely ambiguous cases (laborer attribution, enforcement mechanism) are routed through omegas rather than hard-coded corrections.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping covenant-memory and survival capacity alive after the loss of temple, territory, and priesthood — is still live: dispersion persists, catastrophes recurred (the Holocaust re-founded the problem in blood), and the transmission problem no school or museum has solved as robustly. The mismatch consumer therefore reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag; mandatrophy is not resolved because there is no orphaned mandate. The live risk is the inverse of atrophy: function outrunning recognition — secular households performing the sequence without registering that they are executing survival technology, which is how a functioning rope begins its slide toward piton. Against that drift the arrangement carries an unusual safeguard: the closing line 'next year in Jerusalem' embeds an explicit obsolescence condition inside the practice itself (has_sunset_clause is authored true on that basis), periodically restating the terms on which the current form would end — a self-declared sunset that snares characteristically suppress and pitons characteristically forget. The messianic_sunset_sincerity omega records whether that clause is operative or ornamental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates one reading (hybrid_transformation) of the kernel catastrophe_memory_function; would the sibling readings (mourning_practice, survival_competence) authorize a different epsilon and a different beneficiary structure for the same colloquial label?',
    'Compile all three sibling stories and compare per-seat classifications and epsilon across the family; the divergence locates the disagreement in the kernel''s operative content.',
    'Under the mourning-only sibling, the survival-transmission content leaves the referent (lower functionality claim, narrower beneficiary set); under the survival-only sibling, the mourning register leaves it. The epsilon authored here (0.28) is valid only for the hybrid referent and must not be averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    fusion_vs_superposition,
    'Are the mourning and survival functions fused within single ritual elements (bitter herbs as simultaneous grief-marker and endurance-lesson), or merely superposed as separate elements packed into one evening?',
    'Element-level ritual analysis plus transmission-outcome studies: if removing the mourning elements eliminates survival-transmission efficacy (or vice versa), the functions are fused; if each survives the other''s removal, they are separable.',
    'If separable, the epsilon-invariance principle requires splitting this story into two linked constraints with independent epsilon values; if fused, the hybrid reading is irreducible and this file stands as one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fusion_vs_superposition, empirical, 'Whether the dual function is structurally fused or an aggregation of separable parts.').

omega_variable(
    enforcement_scale_ambiguity,
    'Is the measured suppression structural (communal sanction, family economic and social pull) or internalized (guilt, filial duty, identity fusion that makes non-attendance feel like self-erasure)?',
    'Post-exit trajectory study of lapsed members: if felt obligation decays quickly after exit, suppression was structural; if it persists as guilt and phantom-duty, it is internalized.',
    'Internalized suppression raises effective suppression above the structural measure for identity-locked seats and would push per-seat computations toward heavier enforcement readings; the base scalar (0.22) assumes predominantly internalized/relational carriage (roughly 70 percent) with light structural backing (roughly 30 percent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scale_ambiguity, empirical, 'Structural versus internalized suppression mechanism in a socially enforced ritual obligation.').

omega_variable(
    gendered_labor_attribution,
    'Does the preparation-labor asymmetry borne by seder_preparation_laborers belong to this arrangement, or is it inherited from surrounding household gender norms that the annual evening merely rides on?',
    'Comparative labor accounting within the same households: seder-week allocation versus ordinary festive meals; if the asymmetry appears only at seder-time, the arrangement carries it; if it matches baseline allocation, it is inherited.',
    'If arrangement-specific, a genuine cost-concentration channel exists inside the structure and per-seat computation should weight the laborer seat toward the target end; if inherited, the evening is a neutral carrier and the asymmetry belongs to a different constraint altogether.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gendered_labor_attribution, empirical, 'Attribution of the preparation-labor asymmetry to the ritual versus ambient household norms.').

omega_variable(
    messianic_sunset_sincerity,
    'Does the closing line ''next year in Jerusalem'' operate as a sincere termination condition for the diaspora-form of the practice, or as perpetual aspiration never meant to end it?',
    'Historical natural experiment: practice change after 1948 and 1967, when restoration became partially actual — did communities treat the condition as triggered, or re-read the line''s referent to keep the diaspora-form open-ended?',
    'If sincere, the arrangement carries a real embedded sunset clause and shares transitional DNA despite its age; if not, the line is theatrical and the arrangement is open-ended steady-state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_sunset_sincerity, conceptual, 'Sincerity of the ritual''s own embedded obsolescence condition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 200, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_mem_hybrid_tr_t200, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(catastrophe_mem_hybrid_tr_t600, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(catastrophe_mem_hybrid_tr_t1100, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1100, 0.14).
narrative_ontology:measurement(catastrophe_mem_hybrid_tr_t1500, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1500, 0.16).
narrative_ontology:measurement(catastrophe_mem_hybrid_tr_t1800, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1800, 0.2).
narrative_ontology:measurement(catastrophe_mem_hybrid_tr_t1945, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement(catastrophe_mem_hybrid_tr_t1970, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 1970, 0.16).
narrative_ontology:measurement(catastrophe_mem_hybrid_tr_t2026, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(catastrophe_mem_hybrid_be_t200, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 200, 0.18).
narrative_ontology:measurement(catastrophe_mem_hybrid_be_t600, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 600, 0.2).
narrative_ontology:measurement(catastrophe_mem_hybrid_be_t1100, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1100, 0.34).
narrative_ontology:measurement(catastrophe_mem_hybrid_be_t1500, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1500, 0.38).
narrative_ontology:measurement(catastrophe_mem_hybrid_be_t1800, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1800, 0.24).
narrative_ontology:measurement(catastrophe_mem_hybrid_be_t1945, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement(catastrophe_mem_hybrid_be_t1970, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 1970, 0.26).
narrative_ontology:measurement(catastrophe_mem_hybrid_be_t2026, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_mem_hybrid_su_t200, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 200, 0.25).
narrative_ontology:measurement(catastrophe_mem_hybrid_su_t600, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 600, 0.3).
narrative_ontology:measurement(catastrophe_mem_hybrid_su_t1100, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1100, 0.55).
narrative_ontology:measurement(catastrophe_mem_hybrid_su_t1500, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1500, 0.6).
narrative_ontology:measurement(catastrophe_mem_hybrid_su_t1800, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1800, 0.3).
narrative_ontology:measurement(catastrophe_mem_hybrid_su_t1945, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(catastrophe_mem_hybrid_su_t1970, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(catastrophe_mem_hybrid_su_t2026, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel catastrophe_memory_function. The colloquial label 'the seder commemorates suffering' conflates two structurally distinct functions: preservation of loss-memory through memorial obligation (the mourning sibling's referent) and transmission of adaptive, destruction-resistant practice (the survival sibling's referent). This file instantiates the hybrid reading, in which both functions are claimed as fused in one structure, and accordingly authors a single epsilon (0.28) over the fused referent. The siblings are upstream/downstream of this file analytically rather than causally: whichever function a scholar takes as primary determines which elements count as load-bearing, so the three stories must be compared per-seat rather than merged. Per the epsilon-invariance principle, if the fusion_vs_superposition omega resolves toward separability, this file splits into two linked stories and the family becomes four.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
