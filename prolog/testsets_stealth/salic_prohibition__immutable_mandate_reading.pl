% ============================================================================
% CONSTRAINT STORY: salic_prohibition__immutable_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE — operative law extinct; persists as claimant ideology]
% ============================================================================

:- module(constraint_salic_prohibition__immutable_mandate_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: salic_prohibition__immutable_mandate_reading
 *   human_readable: Salic Prohibition as Immutable Natural/Divine Mandate (Dynastic Fundamental-Law Reading)
 *   domain: constitutional/political/dynastic succession
 *
 * SUMMARY:
 *   The immutable-mandate reading holds that the exclusion of women from
 *   dynastic succession is not a statute but a fundamental law of the realm:
 *   irrevocable, anterior to and superior to any sovereign's will, grounded
 *   in nature and divine ordination as expressed in the ancient Frankish law
 *   and time-immemorial custom. Under this reading the standing arrangement
 *   is: thrones descend exclusively through male lines; claims through
 *   females are void as a class; challengers to female succession act
 *   legitimately; and where a female succession is attempted, preventive war
 *   to restore agnatic priority is justifiable. The arrangement operated
 *   across Capetian, Valois, and Bourbon France and Bourbon Spain from the
 *   1316 exclusion of Joan through the Carlist defeat of 1876. The
 *   claimed_type records the reading's own claim about itself — irrevocable
 *   natural law, a mountain claim — while the metrics record the
 *   arrangement's operation as the historical record shows it, including its
 *   enforcement dependence and eventual collapse; the two are authored
 *   independently, and any divergence between the computed type and the
 *   mountain claim is precisely the false-summit measurement this story
 *   exists to take. Epsilon's referent is the standing agnatic-exclusion
 *   arrangement, assessed by this reading's own lights: the reading prices
 *   the allocation itself as ordination rather than taking, but its own
 *   doctrine concedes the allocation is not self-executing and must be
 *   enforced at recurring cost.
 *
 * KEY AGENTS:
 *   - male_line_dynasts: primary beneficiary (powerful/identity_locked) — collect the succession priority the rule allocates; their dynastic identity is constituted by agnatic descent
 *   - dynastic_jurists_and_parlements: agenda setter (institutional/identity_locked) — articulate, register, and enforce the fundamental laws; their corporate authority is constituted by guardianship
 *   - agnatic_challengers: beneficiary-enforcer (powerful/mobile) — invoke the immutable law to legitimate challenges (Silesia 1740, Carlist risings); commitment is instrumental and droppable
 *   - female_heirs: primary target (powerless/trapped) — categorically barred by sex and descent; no exit exists from the attribute the rule attaches to
 *   - female_line_claimants: target with nominal power (powerful/constrained) — claims through mothers and grandmothers voided as a class (Edward III); power does not purchase exit because the bar attaches to descent
 *   - succession_war_populations: diffuse cost-bearers (powerless/trapped) — taxed and conscripted for enforcement wars declared in councils where they hold no seat
 *   - female_subjects_of_dynastic_states: excluded voice (powerless/trapped) — the constitutional conversation never included them
 *   - reigning_sovereign: dual-positioned (powerful/identity_locked) — elevated by agnatic priority and bound by the same irrevocability; cannot settle the crown on a daughter
 *   - dynastic_constitutional_historians: analytical observer (analytical/analytical) — reconstruct the doctrine's construction, operation, and collapse from outside any dynastic claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__immutable_mandate_reading, 0.45).
domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, 0.45).
domain_priors:theater_ratio(salic_prohibition__immutable_mandate_reading, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, theater_ratio, 0.75).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__immutable_mandate_reading, mountain).
narrative_ontology:human_readable(salic_prohibition__immutable_mandate_reading, "Salic Prohibition as Immutable Natural/Divine Mandate (Dynastic Fundamental-Law Reading)").
narrative_ontology:topic_domain(salic_prohibition__immutable_mandate_reading, "constitutional/political/dynastic succession").

domain_priors:requires_active_enforcement(salic_prohibition__immutable_mandate_reading).
domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__immutable_mandate_reading, '89ce74a7-a3f9-4597-a99c-f278444fcdc9').
narrative_ontology:cs_kernel_codification('89ce74a7-a3f9-4597-a99c-f278444fcdc9', fixed_text).
narrative_ontology:cs_authority_grounding('89ce74a7-a3f9-4597-a99c-f278444fcdc9', lineage).
narrative_ontology:cs_interpretation_layer_present('89ce74a7-a3f9-4597-a99c-f278444fcdc9').
narrative_ontology:cs_reading_relation('89ce74a7-a3f9-4597-a99c-f278444fcdc9', salic_prohibition__sovereign_override_reading, forecloses).
narrative_ontology:cs_reading_relation('89ce74a7-a3f9-4597-a99c-f278444fcdc9', salic_prohibition__cognatic_reversion_reading, forecloses).
narrative_ontology:cs_axiom('89ce74a7-a3f9-4597-a99c-f278444fcdc9', foundational, agnatic_succession_divinely_ordained).
narrative_ontology:cs_axiom_status(agnatic_succession_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('89ce74a7-a3f9-4597-a99c-f278444fcdc9', agnatic_succession_divinely_ordained, theological).
narrative_ontology:cs_axiom('89ce74a7-a3f9-4597-a99c-f278444fcdc9', foundational, succession_beyond_sovereign_amendment).
narrative_ontology:cs_axiom_status(succession_beyond_sovereign_amendment, holdable).
narrative_ontology:cs_axiom_grounding('89ce74a7-a3f9-4597-a99c-f278444fcdc9', succession_beyond_sovereign_amendment, conventional).
narrative_ontology:cs_reference_frame('89ce74a7-a3f9-4597-a99c-f278444fcdc9', time_immemorial_agnatic_divine_ordination).
narrative_ontology:cs_drift_state('89ce74a7-a3f9-4597-a99c-f278444fcdc9', post_1876_liberal_constitutional_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('89ce74a7-a3f9-4597-a99c-f278444fcdc9', '').
narrative_ontology:cs_kernel_id(salic_prohibition__immutable_mandate_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, male_line_dynasts).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, dynastic_jurists_and_parlements).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, agnatic_challengers).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_heirs).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, female_line_claimants).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, succession_war_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__immutable_mandate_reading, reigning_sovereign).
narrative_ontology:constraint_victim(salic_prohibition__immutable_mandate_reading, reigning_sovereign).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The parlement of Paris and the corps of legists and canonists who served it articulated the doctrine that the crown descends only through male lines and that this rule is a fundamental law of the realm anterior to every king. They registered edicts touching the succession and refused registration to arrangements departing from it, and they supplied the learned arguments by which challenges to female succession were legitimated. Their corporate dignity rested on guardianship of these laws; a jurist who conceded that the realm could reconstitute its succession at will would have dissolved his own office.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, dynastic_jurists_and_parlements, agenda_setter,
    institutional, generational, identity_locked, national).

% Princes of the blood in the male line received the crowns and appanages the rule allocates: in 1328 the count of Valois took a throne that proximity of blood would have routed through a woman, and in Spain the infante Don Carlos stood to inherit under the law his brother sought to amend. Their claim to rule is male-line descent itself, so abandoning the rule would dissolve the title by which they hold anything. They also furnished the arms and marriage alliances by which the allocation was defended.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, male_line_dynasts, beneficiary,
    powerful, generational, identity_locked, continental).

% Kings and princes outside the immediate dynasty invoked the immutable law when it opened a path to territory or a crown: the Prussian king pressed the case against the Habsburg female succession in 1740 and took Silesia under its cover, and Spanish Carlists rose repeatedly in its name. The commitment was instrumental — the Prussian king abandoned the legal position once Silesia was secured and later made peace with the empress he had challenged. Exit is easy because for them the law was a tool, not a title.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, agnatic_challengers, beneficiary,
    powerful, biographical, mobile, continental).

% Daughters and granddaughters of kings were barred from succeeding regardless of proximity of blood: Joan of France was set aside in 1316 and again in 1328, and the Habsburg archduchess's accession in 1740 was treated by half of Europe as a vacancy to be contested. The bar attaches to sex and descent, attributes that cannot be changed, so there is no exit from the category the rule excludes. Every defense of a female succession had to be bought with war.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_heirs, payer,
    powerless, generational, trapped, continental).

% Claimants whose descent ran through a mother or grandmother — Edward III through Isabella of France above all — held power and armies comparable to the men who excluded them, yet their claims were voided as a class rather than weighed. The rule binds by blood-line, so capability purchased no exit: the choice was abandoning the claim or prosecuting it by arms against the doctrine's enforcement, which Edward III did at the cost of a generation of war.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_line_claimants, payer,
    powerful, biographical, constrained, continental).

% Peasants and townsmen of France, England, the Austrian lands, the Low Countries, and the Spanish provinces paid the taxes, filled the ranks, and saw their fields burned in wars fought over who should wear a crown — wars declared in dynastic councils where they were represented by no one. Their stake in the agnatic question was nil; their liability for its enforcement was total. Exit meant flight from conscription and requisition, available only to the few.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, succession_war_populations, payer,
    powerless, biographical, trapped, continental).

% The women of the dynastic realms lived under constitutions that made their sex a disability at the summit of the state, and no organ of those constitutions gave them a seat from which to object. They would have asked why the realm's fundamental law takes its shape from their exclusion; the parlements, courts, and councils that spoke for the fundamental laws never put the question to them.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, female_subjects_of_dynastic_states, excluded,
    powerless, biographical, trapped, continental).

% The king reigns by the same agnatic priority the rule secures, and the same rule forbids him to settle the crown on a daughter or through a female line — he is the arrangement's chief beneficiary and one of its bound subjects at once. When Ferdinand VII of Spain tried in 1830 to amend the succession so his daughter could inherit, his brother invoked the immutable law against the amendment, and the realm fought a civil war over which of them the constitution bound.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, reigning_sovereign, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__immutable_mandate_reading, reigning_sovereign, payer).

% Historians and legal scholars reconstruct how the succession doctrine was built — from a Frankish private-law text, through the crises of 1316 and 1328, into the fundamental-law tradition — and how it operated, was enforced, and collapsed. They hold no dynastic claim and occupy no seat in any of the arrangements they study.
narrative_ontology:constraint_stakeholder(salic_prohibition__immutable_mandate_reading, dynastic_constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__immutable_mandate_reading, male_line_dynasts).
narrative_ontology:fixing_cost_class(salic_prohibition__immutable_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rule solved a real coordination problem for the dynastic realm: it fixed, in advance and publicly, who would wear the crown when a king died without sons, closing the field against partition among many candidates, against foreign kings pressing descent through females, and against the litigation of every ambitious cousin. One determinate answer — the nearest male — replaced an open question that had twice (1316, 1328) threatened the realm's integrity. It also coordinated the dynasty's marriage policy and the nobility's expectations around a known succession.
% TRANSFER_FUNCTION: It moves sovereignty itself: crowns and the lands attached to them, which proximity of blood would route through a woman, are diverted to the nearest male-line dynast. It also moves the costs of defending that diversion — taxation, conscription, devastation — from the dynasts who hold the allocation to the subject populations that fund its enforcement.
% ABSENT_VOICES: Women of the dynastic realms — heirs and subjects alike — had no seat anywhere the succession was debated; the parlements and councils that spoke for the fundamental laws were entirely male and spoke about women with no woman present. The subject populations who would bear each enforcement war were equally unrepresented in the councils that declared them. The unanimity with which the law's bindingness was affirmed was thus purchased in a room from which every payer had been excluded.
% DISAPPEARANCE_RATIONALE: If the rule and its enforcement vanished overnight at any point in the interval, the successions resolve differently: Joan of France succeeds in 1316 and 1328, the strongest claim of 1328 is Edward III's through his mother, the Habsburg accession of 1740 goes unchallenged, and the Spanish crown passes to Isabella in 1833 without a Carlist war. The dynastic map of Europe, the marriage alliances built around agnatic expectation, and the juristic office of fundamental-law guardianship all reorganize; three of the interval's great wars lose their immediate cause.
% FOUNDING_PROBLEM: The Capetian realm of the early fourteenth century faced a repeated crisis: kings dying without sons (Louis X in 1316, Charles IV in 1328), a field of claimants including a foreign king pressing rights through a female, and the standing threat that the realm would be partitioned or pass to a foreign dynasty. The agnatic exclusion was built to answer one question — who succeeds when there is no son — with a single determinate, dispute-proof answer.
% FOUNDING_PROBLEM_CORROBORATION: No polity governed by this reading remains: the French doctrine was abolished with the dynastic constitution it guarded (1789–1791) and no restoration revived it as operative law; Spain's succession is regulated by written constitutional law under which the present royal house descends through a woman. Constitutional historians, working outside every dynastic interest, attest that the founding problem died with the absolute dynastic realm it served. The only parties still attesting it live are the claimant movements themselves — Carlism and French legitimism — whose own succession claims depend on the problem being alive; no corroborating source outside the beneficiary set attests liveness.
narrative_ontology:disappearance_verdict(salic_prohibition__immutable_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__immutable_mandate_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__immutable_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__immutable_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__immutable_mandate_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__immutable_mandate_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, ExtMetricName, E),
    domain_priors:suppression_score(salic_prohibition__immutable_mandate_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(salic_prohibition__immutable_mandate_reading),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(salic_prohibition__immutable_mandate_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(salic_prohibition__immutable_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time points are years elapsed from 1316: t0 = the exclusion of Joan; t21 = outbreak of the Hundred Years' War; t397 = the Spanish Auto Acordado era; t424 = the Silesian invasion and War of the Austrian Succession; t473 = the French revolutionary abolition of the doctrine; t514 = the Spanish Pragmatic Sanction and Carlist mobilization; t560 = the end of the Third Carlist War. Epsilon (0.45 at interval end) is authored from this reading's own lights: the reading prices the agnatic allocation as ordination rather than taking, but its own framework must count the enforcement costs it concedes and justifies, so the epsilon trajectory tracks enforcement intensity — troughs in settled centuries, peaks at each enforcement war (0.62 at t424, 0.66 at t514), and decline to 0.45 after enforcement capacity breaks in 1876. The suppression_requirement series is authored because the story specifically traces enforcement-capacity change: juristic consolidation (t0–t100), war enforcement (t21, t424), collapse in France at t473 while the Spanish enforcement machinery peaks at t514, and decay after t560. Theater rises monotonically with a dip at each enforcement war (functional activity displaces performance): 0.20 at the doctrine's founding work, 0.75 at interval end, when the reading persists only as legitimist performance. The enforcement pattern is episodic rather than monotonic — war, settlement, accumulation, war — and the oscillation itself functions as intermittent reinforcement: each succession crisis re-demonstrated the law's bindingness to a generation that had not paid for the lesson. All three metric series run on one shared ten-point grid; every metric is authored at every point. End-state scalars match the t560 values. Coalition among the powerless victims was structurally unavailable: the victims were dispersed across realms and generations with no shared forum, and the one female succession defended successfully (1740) succeeded by converting a targeted heir into a powerful sovereign with an army and allies — not by coalition of the powerless. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently. From the jurists' and dynasts' positions the arrangement is the divinely ordained constitution they administer and embody; from the female heirs' position it is categorical dispossession attaching to an unchangeable attribute; from the war populations' position it is conscription and taxation for a quarrel about descent in which they held no stake. The challenger seat diverges from both: agnatic challengers are beneficiaries with arbitrage-grade exit whose commitment is instrumental — the Prussian king dropped the legal position within five years of invoking it — so their seat should compute nearer the beneficiary end than their enforcement role suggests. Same-level divergence: female_line_claimants held power comparable to the male dynasts who excluded them (Edward III versus Philip VI), yet the constraint bound them and not their rivals; the differentiating factor is descent, not capability — the rule binds by blood-line, so nominal power purchases no exit. The reigning_sovereign is genuinely dual-positioned: the derived derivation from his beneficiary role and identity-locked exit would place him deep on the beneficiary side, and that placement is broadly right, but the binding is real (he could not settle the crown on a daughter, and the 1830 attempt triggered civil war); the secondary payer role records this without a scalar override, since an override keyed to his power atom would also misstate the powerful victim seat. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct ordering for every seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: male_line_dynasts collect the diverted successions; dynastic_jurists_and_parlements collect the authority and corporate dignity the guardianship role confers; agnatic_challengers collect territory and crowns under the law's cover, with mobile exit keeping them nearest the beneficiary end. Victims derive high directionality: female_heirs are trapped (the excluded attribute is sex and descent, unchangeable) and sit at the full-target end; female_line_claimants are powerful but constrained — their exit is war or renunciation; succession_war_populations are trapped and bear the enforcement costs with zero agenda power, the most purely targeted seat in the structure. Identity-lock differentiates the beneficiaries too: dynasts and jurists are identity_locked (their titles and offices are constituted by the rule), while challengers are mobile (the law is their instrument, not their identity) — the same nominal power level, opposite exit structures. The gain_flow names male_line_dynasts because the diverted crowns demonstrably accrued there (Valois 1328, the Carlist claim, the Silesian prize); the jurists collected authority rather than territory and are beneficiaries without being the receipt seat. Fixing cost is prohibitive on its own evidence: every amendment attempt (1713, 1830) triggered war, and the seats with amendment power were identity-locked into guardianship of the very rule they would have to break.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — determinate succession for an absolute dynastic realm facing partition and foreign claimants — was real, and the rule solved it for centuries; the classification machinery must not mislabel that genuine coordination component as pure extraction, which is why the coordination function is stated plainly and the boltzmann coordination type (enforcement_mechanism) is declared. But the problem died with the dynastic constitution itself: no polity now governed by this reading exists, and the reading persists as Carlist and legitimist ideology — maintenance of performance after the death of function, visible in the theater_ratio trajectory (0.20 to 0.75) and in the founding_problem_status (dead) set against disappearance_verdict (world_rearranges), a mismatch that flags the zombie pattern for cross-check against the computed theater path. The classification prevents the inverse error as well: the reading's own mountain claim would mislabel a constructed, beneficiary-bearing, enforcement-dependent arrangement as natural law. The natural-law-versus-constructed-rule omega carries exactly that ambiguity for resolution, and the declared beneficiaries on a mountain claim route the story through false-summit evaluation rather than letting the claim self-certify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_rule,
    'Is the agnatic exclusion a genuine natural/divine ordination (as this reading claims) or a constructed dynastic rule serving identifiable male-line interests?',
    'Textual and comparative analysis: the lex Salica provision invoked concerns private allodial land inheritance, not crowns; the succession doctrine first appears in juristic writing at exactly the 1316/1328 crises when the male line needed it; no polity outside the Frankish-derived dynasties independently generated the rule. If the rule tracks interest rather than any religion-independent natural order, the ordination claim fails.',
    'If constructed, the mountain claim is a false summit and the arrangement classifies from its beneficiary/victim structure (a coordinated allocation with asymmetric costs); if genuine natural law, the exclusion would need no enforcement, and the reading''s own repeated recourse to war is anomalous for a self-executing order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rule, conceptual, 'Whether the immutable-mandate claim reflects natural law or constructed dynastic interest.').

omega_variable(
    kernel_reading_indexing,
    'This constraint is the immutable_mandate_reading of the salic_prohibition kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'The sibling stories author the same standing arrangement under different premises: salic_prohibition__sovereign_override_reading treats the exclusion as revocable positive law at the sovereign''s legislative disposal (the Pragmatic Sanction tradition, 1713 and 1830); salic_prohibition__cognatic_reversion_reading treats it as a Frankish anachronism never properly binding outside Frankish territory.',
    'Classification and epsilon are reading-indexed: this reading authors the irrevocability machinery and the war justification as part of the constraint itself; the override reading drops the irrevocability machinery (lower suppression, fixable by ordinary legislation); the cognatic reading authors substantially higher extraction (an illegitimate categorical exclusion imposed by interested parties). The disagreement is located in the source of the law''s bindingness: divine/natural ordination versus sovereign enactment versus customary reception.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexing, conceptual, 'Committer structure: reading-indexed classification over a shared kernel.').

omega_variable(
    war_cost_attribution,
    'Are the succession wars (the Hundred Years'' War, the War of the Austrian Succession, the Carlist civil wars) attributable to this reading''s arrangement, or to geopolitical rivalry that any succession rule would have produced?',
    'Counterfactual succession analysis: under proximity-of-blood succession, Edward III holds the strongest claim of 1328 and the war''s immediate legal cause dissolves; Maria Theresa faces no doctrinal challenge in 1740; the Carlist rising of 1833 has no legal basis. Where great-power rivalry survives the counterfactual independently of the succession rule, attribution is partial.',
    'Full attribution loads the war costs onto this constraint''s account, raising its effective extraction and suppression substantially; partial attribution confines the reading''s own contribution to the exclusion itself and the enforcement episodes it uniquely caused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_cost_attribution, empirical, 'Whether enforcement-war costs belong to this constraint''s account.').

omega_variable(
    irrevocability_sincerity,
    'Did the reading''s holders treat irrevocability as a binding limit on themselves, or invoke it instrumentally only when it served agnatic interests?',
    'Track each invocation against the invoker''s contemporaneous interest: the Prussian king pressed the case against the Habsburg female succession until Silesia was secured at Dresden (1745), then dropped the legal position and later made peace with the empress he had challenged; French jurists held renunciations void under the same fundamental law when French interest required it and binding when Spanish claims threatened it; Spanish Carlists, by contrast, maintained the invocation across a century of military defeat.',
    'If invocation is systematically interest-tracking, the irrevocability claim operates as a pretext instrument at the challenger seats and their directionality sits nearer the target end than their beneficiary declaration suggests; if sincerely held (the Carlist pattern), the mandate reading is a genuine commitment structure whose enforcement costs are internal to its holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irrevocability_sincerity, empirical, 'Whether irrevocability was a lived commitment or an interest-tracking instrument.').

omega_variable(
    founding_problem_liveness,
    'Does the founding problem — determinate succession for an absolute dynastic realm facing partition or foreign claimants — still exist anywhere this reading governs, or is it dead with the arrangement persisting only as claimant ideology?',
    'Survey current polities and claimant movements: no state''s succession law embodies immutable agnatic divine-law exclusion; Carlism and French legitimism persist as movements without a governing object; every extant European monarchy regulates succession by written statute.',
    'A dead founding problem with a persisting arrangement records mandatrophy: the reading''s continued assertion is maintenance of performance rather than function. A live founding problem somewhere would restore the genuineness of the coordination component and support the reading''s own framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether the mandate''s founding problem survives anywhere it governs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__immutable_mandate_reading, 0, 560).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__immutable_mandate_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sali_tr_t21, salic_prohibition__immutable_mandate_reading, theater_ratio, 21, 0.24).
narrative_ontology:measurement(sali_tr_t100, salic_prohibition__immutable_mandate_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(sali_tr_t200, salic_prohibition__immutable_mandate_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement(sali_tr_t300, salic_prohibition__immutable_mandate_reading, theater_ratio, 300, 0.4).
narrative_ontology:measurement(sali_tr_t397, salic_prohibition__immutable_mandate_reading, theater_ratio, 397, 0.45).
narrative_ontology:measurement(sali_tr_t424, salic_prohibition__immutable_mandate_reading, theater_ratio, 424, 0.38).
narrative_ontology:measurement(sali_tr_t473, salic_prohibition__immutable_mandate_reading, theater_ratio, 473, 0.7).
narrative_ontology:measurement(sali_tr_t514, salic_prohibition__immutable_mandate_reading, theater_ratio, 514, 0.55).
narrative_ontology:measurement(sali_tr_t560, salic_prohibition__immutable_mandate_reading, theater_ratio, 560, 0.75).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__immutable_mandate_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sali_be_t21, salic_prohibition__immutable_mandate_reading, base_extractiveness, 21, 0.42).
narrative_ontology:measurement(sali_be_t100, salic_prohibition__immutable_mandate_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(sali_be_t200, salic_prohibition__immutable_mandate_reading, base_extractiveness, 200, 0.44).
narrative_ontology:measurement(sali_be_t300, salic_prohibition__immutable_mandate_reading, base_extractiveness, 300, 0.46).
narrative_ontology:measurement(sali_be_t397, salic_prohibition__immutable_mandate_reading, base_extractiveness, 397, 0.52).
narrative_ontology:measurement(sali_be_t424, salic_prohibition__immutable_mandate_reading, base_extractiveness, 424, 0.62).
narrative_ontology:measurement(sali_be_t473, salic_prohibition__immutable_mandate_reading, base_extractiveness, 473, 0.55).
narrative_ontology:measurement(sali_be_t514, salic_prohibition__immutable_mandate_reading, base_extractiveness, 514, 0.66).
narrative_ontology:measurement(sali_be_t560, salic_prohibition__immutable_mandate_reading, base_extractiveness, 560, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__immutable_mandate_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sali_su_t21, salic_prohibition__immutable_mandate_reading, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(sali_su_t100, salic_prohibition__immutable_mandate_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(sali_su_t200, salic_prohibition__immutable_mandate_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement(sali_su_t300, salic_prohibition__immutable_mandate_reading, suppression_requirement, 300, 0.55).
narrative_ontology:measurement(sali_su_t397, salic_prohibition__immutable_mandate_reading, suppression_requirement, 397, 0.6).
narrative_ontology:measurement(sali_su_t424, salic_prohibition__immutable_mandate_reading, suppression_requirement, 424, 0.72).
narrative_ontology:measurement(sali_su_t473, salic_prohibition__immutable_mandate_reading, suppression_requirement, 473, 0.5).
narrative_ontology:measurement(sali_su_t514, salic_prohibition__immutable_mandate_reading, suppression_requirement, 514, 0.75).
narrative_ontology:measurement(sali_su_t560, salic_prohibition__immutable_mandate_reading, suppression_requirement, 560, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__immutable_mandate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__sovereign_override_reading).
narrative_ontology:affects_constraint(salic_prohibition__immutable_mandate_reading, salic_prohibition__cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Salic law' conflates three structurally distinct claims about the same historical rule, decomposed here per the epsilon-invariance principle into a three-story constraint family. This story instantiates the immutable-mandate reading (irrevocable natural/divine law); the siblings instantiate the sovereign-override reading (revocable positive law, epsilon authored lower, no irrevocability machinery) and the cognatic-reversion reading (non-binding anachronism, epsilon authored substantially higher). All three share one referent — the standing agnatic-exclusion arrangement — and diverge only in reading-indexed evaluation. This reading is downstream of neither sibling historically; it is the position against which both siblings defined themselves, and its enforcement wars are the pressure it applied to their operating environments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
