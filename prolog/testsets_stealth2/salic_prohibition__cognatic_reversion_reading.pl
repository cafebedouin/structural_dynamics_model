% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Prohibition — Cognatic Reversion Reading (Frankish Anachronism)
 *   domain: constitutional/dynastic/political_history
 *
 * SUMMARY:
 *   The Salic prohibition — the bar on female succession attributed to the
 *   ancient Salian Franks — structured European dynastic politics from the
 *   fourteenth century onward. This story instantiates ONE reading of the
 *   contested salic_prohibition kernel: the cognatic reversion reading, which
 *   holds that the rule was Frankish customary law never properly extended
 *   beyond its original jurisdiction, hence void for non-Frankish realms, and
 *   that where territorial integrity and agnatic purity conflict, integrity
 *   prevails — yielding succession by cognatic primogeniture. Per the
 *   epsilon-referent rule for kernel readings, base_properties author epsilon
 *   for the STANDING ARRANGEMENT UNDER CONTEST — the prohibition's operation
 *   over non-Frankish successions — as this reading assesses it: an
 *   improperly authoritative rule sustained by interested enforcement,
 *   transferring succession rights from female lines to agnatic males and
 *   imposing recurrent war costs on realm populations. The sibling readings
 *   (immutable_mandate_reading, sovereign_override_reading) are separate
 *   constraints with their own epsilon, beneficiaries, and classifications,
 *   linked through network.affects_constraints; the claim/metric pair here is
 *   authored independently — the snare claim states this reading's structural
 *   assessment, and the metrics state the prohibition's observed operation,
 *   without reconciling either to predicted engine output.
 *
 * KEY AGENTS:
 *   - - dynastic_courts_and_parlements: Agenda-setting administrator (institutional/identity_locked) — articulates and enforces the fundamental-law doctrine; their judicial authority rests on the doctrine's continuity
 *   - - agnatic_male_line_claimants: Primary beneficiary (organized/constrained) — collects crowns, regencies, and claim-value the exclusion reserves for the male line
 *   - - female_line_heirs: Primary target (moderate/trapped) — stands first in bloodline with no procedural route to exercise
 *   - - realm_populations_bearing_succession_wars: Diffuse payer (powerless/trapped) — funds and fills the armies each succession contest raises
 *   - - legitimist_traditionalists: Ideological beneficiary (organized/identity_locked) — the rule's inviolability is load-bearing in their constitutional theology
 *   - - royal_consort_households: Secondary beneficiary (powerful/mobile) — gain crowns and precedence when reversion succeeds
 *   - - foreign_guarantor_powers: Institutional observer (institutional/analytical) — guarantee settlements, weigh recognition
 *   - - constitutional_historians: Analytical observer (analytical/analytical) — document the rule's constructed genealogy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.7).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.58).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.53).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.53).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, snare).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Prohibition — Cognatic Reversion Reading (Frankish Anachronism)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional/dynastic/political_history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '514acb3b-ac2f-4469-8d89-b92e32325388').
narrative_ontology:cs_kernel_codification('514acb3b-ac2f-4469-8d89-b92e32325388', fixed_text).
narrative_ontology:cs_authority_grounding('514acb3b-ac2f-4469-8d89-b92e32325388', lineage).
narrative_ontology:cs_interpretation_layer_present('514acb3b-ac2f-4469-8d89-b92e32325388').
narrative_ontology:cs_reading_relation('514acb3b-ac2f-4469-8d89-b92e32325388', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('514acb3b-ac2f-4469-8d89-b92e32325388', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('514acb3b-ac2f-4469-8d89-b92e32325388', foundational, salic_binding_terminates_at_frankish_jurisdiction).
narrative_ontology:cs_axiom_status(salic_binding_terminates_at_frankish_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('514acb3b-ac2f-4469-8d89-b92e32325388', salic_binding_terminates_at_frankish_jurisdiction, empirically_contingent).
narrative_ontology:cs_axiom('514acb3b-ac2f-4469-8d89-b92e32325388', foundational, territorial_integrity_privileged_over_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_privileged_over_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('514acb3b-ac2f-4469-8d89-b92e32325388', territorial_integrity_privileged_over_agnatic_purity, instrumental).
narrative_ontology:cs_reference_frame('514acb3b-ac2f-4469-8d89-b92e32325388', frankish_customary_law_original_scope).
narrative_ontology:cs_drift_state('514acb3b-ac2f-4469-8d89-b92e32325388', fourteenth_century_valois_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('514acb3b-ac2f-4469-8d89-b92e32325388', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, agnatic_male_line_claimants).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, legitimist_traditionalists).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, female_line_heirs).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, realm_populations_bearing_succession_wars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, royal_consort_households).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, agnatic_exclusion_doctrine).
narrative_ontology:constraint_vindicates(salic_prohibition__cognatic_reversion_reading, fundamental_law_inviolability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign courts and parlements of each realm hear succession litigation, register or refuse royal acts touching the crown's descent, and articulate the doctrine that exclusion of female succession is an immemorial fundamental law of the kingdom. Their judicial authority rests on the continuity of the received doctrine; treating it as a recent or foreign construction would unsettle the foundations of their own office. Individual judges can dissent, but the institution as such has no path to repudiating the doctrine without repudiating its own pedigree.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, dynastic_courts_and_parlements, agenda_setter,
    institutional, generational, identity_locked, national).

% Collateral male relatives of reigning houses hold seniority in the line of succession wherever the exclusion operates. When a king dies without sons, the nearest agnate collects the crown, the regency, or the marriage-market value of the expectation. Some accept compensation to renounce, as Infante Carlos Luis did in 1845; most treat the claim as family patrimony to be defended across generations, by litigation where possible and by arms where necessary.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_male_line_claimants, beneficiary,
    organized, generational, constrained, continental).

% Daughters and sisters of kings stand first in bloodline but find their rights unenforceable: registries refuse their titles, councils exclude them from regencies, and no procedural route leads from barred heir to crowned sovereign short of a sovereign's extraordinary act or a victorious war. Maria Theresa's path ran through a European coalition war; Isabella II's through her father's last-minute pragmatic sanction and a decade of civil war fought in her name.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_line_heirs, payer,
    moderate, biographical, trapped, continental).

% Peasants, townsmen, and taxpayers of the contested realms fund and fill the armies raised whenever the succession is disputed. They gain nothing from the exclusion itself — the crown passes between the same handful of dynasts regardless — but they bear the requisitions, conscriptions, and devastations of each succession war, from the opening phase of the Hundred Years' War to the Carlist campaigns of the 1830s through 1870s.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, realm_populations_bearing_succession_wars, payer,
    powerless, immediate, trapped, national).

% Jurists, clergy, and militant parties who hold the exclusion to be divinely or naturally ordained defend it in print, in the cortes, and in the field. The rule's inviolability is a load-bearing wall of their constitutional theology; conceding that it was a local Frankish custom wrongly generalized would collapse their account of legitimate order. Their mobilization, most durably Spanish Carlism, fused political program with religious identity, leaving compromise structurally unavailable to them.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, legitimist_traditionalists, beneficiary,
    organized, generational, identity_locked, national).

% Dynastic houses that marry their sons to female heirs stand to gain crowns, territories, and precedence when the exclusion is lifted. The House of Lorraine's elevation through Francis Stephen's marriage to Maria Theresa is the paradigm: a cadet branch converted into a great dynasty by the reversion it supported. Their mobility lies in redeploying marriage alliances across courts as legitimacy conditions shift.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, royal_consort_households, beneficiary,
    powerful, generational, mobile, continental).

% Great powers guarantee succession settlements such as Utrecht and Vienna and decide recognition of claimants. They take no side in the doctrine as such but weigh each contested succession for balance-of-power effect, extending or withholding recognition as the settlement serves their interests. Their archives record the doctrinal arguments chiefly as negotiating positions.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, foreign_guarantor_powers, observer,
    institutional, generational, analytical, continental).

% Medievalists and legal historians reconstruct the rule's genealogy from manuscript and archival sources: the Pactus Legis Salicae's actual contents, the fourteenth-century French adaptation that first deployed Salic language to bar claims transmitted through women, and the subsequent export of the label to other courts. Their work supplies the reception history on which any judgment of the rule's proper scope must rest.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, agnatic_male_line_claimants).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a determinate answer to the succession question — who takes the crown when a king dies without sons — intended to prevent partition, preempt competing claims, and close the route by which a foreign prince might inherit through his mother. Any clear succession rule performs the first part; the exclusionary content is specific to this arrangement.
% TRANSFER_FUNCTION: Moves succession rights — and with them crowns, regencies, territories, and marriage-market value — from female-line heirs to the nearest agnatic males; moves the costs of enforcing and contesting the rule (litigation, subsidy, war) onto royal treasuries and realm populations.
% ABSENT_VOICES: Female heirs and their mothers had no seat in the councils, courts, and doctrinal traditions that articulated and administered the rule; the populations who funded its wars appeared only as taxable bodies. Both groups would have objected to the rule's content and its costs; their objections enter the record mainly through the wars fought in their names or against their interests.
% DISAPPEARANCE_RATIONALE: Overnight disappearance rearranges every dynasty's succession order: female-line heirs ascend in France (Jeanne II in 1316), Spain, Portugal, and the imperial succession; the Hundred Years' War loses its dynastic trigger; the War of Austrian Succession and the Carlist Wars do not occur as fought; marriage-alliance diplomacy reorganizes around different expectant houses.
% FOUNDING_PROBLEM: Two founding layers: the Pactus Legis Salicae (early sixth century) addressed composition payments and allodial landholding among Salian Franks and contained no provision on succession to crowns; the succession application was built in fourteenth-century France during the crisis following Louis X's death (1316) and consolidated against Edward III's claim through his mother, to keep the crown within the Capetian male line and out of English hands.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the philological record itself: extant Lex Salica manuscripts contain no provision on royal succession, and the first systematic deployment of Salic succession doctrine appears in fourteenth-century French polemic, as documented in the modern scholarship of Giesey, Taylor, and Soria on the construction of the Salic law of succession. The courts' immemorial-fundamental-law formula is precisely what the manuscript evidence contradicts; no beneficiary attests the founding problem accurately.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because the rule transfers whole succession orders — crowns, regencies, territories — from female lines to agnatic males, with the transfer decoupled from any service rendered to those displaced. Suppression (0.58, decaying from a Carlist-era peak of 0.84) reflects enforcement that always exceeded paperwork: registry refusals, council exclusions, and three civil wars. Theater ratio (0.53 and rising) captures the growing share of activity that is ceremonial invocation: citing a sixth-century wergeld-and-land code as the constitutional foundation of eighteenth- and nineteenth-century successions is literal anachronism-theater, though enforcement remained materially real through the nineteenth century, keeping theater below piton range. Accessibility collapse is moderate (0.38): once the genealogy is seen, the rule's necessity-claim dissolves — cognatic statutes proved workable wherever enacted — but practical instantiation required extraordinary sovereign acts and great-power acquiescence, so alternatives never collapsed into easy availability. Resistance is high (0.72): Jeanne of Navarre's partisans, Edward III's claim, the War of Austrian Succession, and the Carlist Wars. The measurement series run on ONE shared grid (seven points, all three metrics at each) per the alignment rule; trajectories show extraction and suppression peaking around the Carlist crisis (t=25) then decaying as cognatic adoption spreads. Suppression is authored as a raw structural property (unscaled); extractiveness is the quantity the engine scales by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is sharp. From the courts' seat the rule IS their authority — its continuity constitutes the office, and repudiating the doctrine would repudiate their own pedigree (identity_locked exit). From the agnatic claimants' seat it is family patrimony defended across generations. From the female heirs' seat it is dispossession administered by procedure. From the populations' seat it is a war generator with no payoff. Two same-level contrasts refine this: dynastic courts and foreign guarantor powers both hold institutional power, but the courts are identity_locked insiders while the guarantors hold analytical exit and treat the doctrine as a negotiating position; agnatic claimants and legitimist traditionalists are both organized, but the claimants' exit is constrained (renunciation-with-compensation was occasionally taken) while the legitimists' identity fusion makes compromise structurally unavailable. The engine computes per-seat classifications from this structural data; the authored snare claim adjudicates nothing at seat level.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (agnatic_male_line_claimants, legitimist_traditionalists) derive low directionality — the rule subsidizes them, damping or inverting their effective extraction. Declared victims (female_line_heirs, realm_populations_bearing_succession_wars) derive high directionality, amplified by trapped exits: a princess with no procedural route and a taxpayer with no exit sit near the full-target end. The dynastic courts are deliberately NOT declared beneficiaries: their profit is authority rather than revenue, and they also spend enforcement capital, so the canonical fallback places them mid-range rather than at the beneficiary pole — an approximation noted here rather than forced with an override, since the override surface keys on power atoms and would sweep the guarantor powers with the courts. Continental scope on the dynastic seats modestly amplifies effective extraction through verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview locates the mandatrophy cleanly: both founding problems are dead — the Pactus Legis Salicae's actual subjects (composition tariffs, allodial land) expired with the Frankish kingdom, and the fourteenth-century problem the succession doctrine was built to solve (keeping the crown from Edward III) expired with the Plantagenet claim — yet the arrangement persisted into the twentieth century. founding_problem_status=dead combined with disappearance_verdict=world_rearranges is exactly the mismatch profile that flags capture/zombie persistence, cross-checked against theater_ratio's monotonic rise (0.28 to 0.53). The classification guards both directions of mislabeling: claiming snare rather than accepting the rule's self-description as timeless fundamental law prevents laundering extraction as natural order (the immutable_mandate sibling would make precisely that mountain-claim); equally, acknowledging that ANY determinate succession rule carries a generic coordination function prevents over-reading the snare as contentless — this reading's own position is that the coordination was available without the exclusionary content, which is why the exclusion, not the ordering, is the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the cognatic_reversion_reading of the salic_prohibition kernel; what changes structurally if the immutable_mandate_reading or the sovereign_override_reading governs instead?',
    'Observe which reading''s axioms a polity''s constitutional texts and court doctrine actually adopt: immutable_mandate asserts naturality and forecloses legislative repair; sovereign_override treats the rule as enactable and repealable positive law; this reading voids it ab initio outside the original Frankish jurisdiction.',
    'Under immutable_mandate the arrangement presents as a natural-law claim with no authorized exit (mountain-flavored, FSM-relevant if beneficiaries surface); under sovereign_override it becomes an amendable statutory arrangement (rope/scaffold-flavored); under this reading it is an improperly authoritative imposition (snare-flavored). Classification of the same historical arrangement tracks the adopted reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega: per-reading structural delta across the salic_prohibition kernel.').

omega_variable(
    reception_history_scope,
    'Which non-French realms ever expressly received the succession exclusion (by statute, auto acordado, or sworn fundamental oath), and which inherited it silently as borrowed rhetoric?',
    'Archival reception study: compare express enactments (Castilian Auto Acordado of 1713, Portuguese constitutional charters, Habsburg house-law instruments) against realms showing only rhetorical citation; map the bindingness claim realm by realm.',
    'Where express reception exists, the never-properly-binding claim narrows to non-receiving realms and this reading''s epsilon drops for receivers; where reception is purely rhetorical, the void-ab-initio claim holds in full and epsilon stays high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_history_scope, empirical, 'Territorial extent of express versus rhetorical reception of the exclusion.').

omega_variable(
    anachronism_vs_construction,
    'Is the succession exclusion better described as a misapplied anachronism (a rule once valid somewhere, wrongly generalized) or a pure political construction (never valid anywhere as succession law)?',
    'Philological comparison of the Pactus Legis Salicae''s actual provisions against the fourteenth-century succession doctrine attributed to it, tracing the interpretive move that converted a composition-tariff and allodial-land code into a constitutional principle.',
    'Pure construction strengthens the void-ab-initio claim (the rule never had authority anywhere as succession law, deepening the snare reading); misapplied anachronism leaves residual authority inside the original jurisdiction and softens the reading''s scope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anachronism_vs_construction, conceptual, 'Framing ambiguity in the rule''s genealogy: misapplication versus invention.').

omega_variable(
    integrity_priority_stability,
    'Does the reading''s priority of territorial integrity over agnatic purity hold uniformly, or does it yield when the cognate heir''s consort is a foreign prince positioned to absorb the realm?',
    'Comparative case outcomes: Austrian acceptance of Maria Theresa (whose consort led no rival great power) versus French and Spanish resistance to heiresses whose husbands headed foreign crowns; code the priority''s stability across the recorded cases.',
    'A stable priority supports the reading as a coherent rule; case-by-case yielding fragments it into interested exceptions, raising measured extractiveness toward agnatic claimants and exposing the integrity rationale as selective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integrity_priority_stability, empirical, 'Uniformity of the territorial-integrity-over-agnatic-purity commitment.').

omega_variable(
    enforcement_decay_trajectory,
    'Does the post-Carlist decay of enforcement continue toward inertial ceremonial maintenance, or does neo-legitimist revival re-erupt armed enforcement?',
    'Track legitimist mobilization and court doctrine after 1876: the third Carlist defeat, the Alfonsine restoration settlement, twentieth-century pretender activity, and any doctrinal revival during constitutional crises.',
    'Continued decay pushes the arrangement toward piton-like performance (rising theater, falling suppression); re-eruption restores snare dynamics with renewed suppression and extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Trajectory of enforcement decay versus revival after the Carlist defeat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(sali_tr_t0, observed).
narrative_ontology:measurement(sali_tr_t5, salic_prohibition__cognatic_reversion_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(sali_tr_t5, observed).
narrative_ontology:measurement(sali_tr_t10, salic_prohibition__cognatic_reversion_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(sali_tr_t10, observed).
narrative_ontology:measurement(sali_tr_t15, salic_prohibition__cognatic_reversion_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(sali_tr_t15, observed).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__cognatic_reversion_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(sali_tr_t20, observed).
narrative_ontology:measurement(sali_tr_t25, salic_prohibition__cognatic_reversion_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement_basis(sali_tr_t25, observed).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__cognatic_reversion_reading, theater_ratio, 30, 0.53).
narrative_ontology:measurement_basis(sali_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(sali_be_t0, observed).
narrative_ontology:measurement(sali_be_t5, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(sali_be_t5, observed).
narrative_ontology:measurement(sali_be_t10, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(sali_be_t10, observed).
narrative_ontology:measurement(sali_be_t15, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(sali_be_t15, observed).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(sali_be_t20, observed).
narrative_ontology:measurement(sali_be_t25, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(sali_be_t25, observed).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(sali_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(sali_su_t0, observed).
narrative_ontology:measurement(sali_su_t5, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(sali_su_t5, observed).
narrative_ontology:measurement(sali_su_t10, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(sali_su_t10, observed).
narrative_ontology:measurement(sali_su_t15, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement_basis(sali_su_t15, observed).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(sali_su_t20, observed).
narrative_ontology:measurement(sali_su_t25, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 25, 0.84).
narrative_ontology:measurement_basis(sali_su_t25, observed).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(sali_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Salic Law': the label conflates at least three structurally distinct claims about the rule's normative source and scope, with materially different epsilon values. This story instantiates the cognatic_reversion_reading (local Frankish custom, void outside its jurisdiction; epsilon authored high for the prohibition's extra-jurisdictional operation). The immutable_mandate_reading (universal divine/natural law; low epsilon, natural-law certification attempted) and the sovereign_override_reading (revocable positive law; intermediate epsilon, amendable arrangement) are separate stories. Citation pressure runs from the immutable reading's fundamental-law rhetoric into the terrain where the other two readings contend; all three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
