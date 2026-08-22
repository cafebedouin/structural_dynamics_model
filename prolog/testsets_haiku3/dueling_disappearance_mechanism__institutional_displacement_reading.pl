% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling as Institutionally-Displaced Dispute Resolution Protocol
 *   domain: legal_history/institutional_anthropology
 *
 * SUMMARY:
 *   This constraint instantiates the institutional-displacement reading of
 *   dueling's historical disappearance. Dueling coordinated dispute
 *   resolution when formal legal institutions were weak, inaccessible, or
 *   lacked legitimacy to adjudicate matters of honor. As courts expanded,
 *   commercial banking developed legal frameworks for contract disputes, and
 *   libel law codified reputational harm as a civil cause of action, dueling
 *   became structurally redundant. The reading treats this as a voluntary
 *   substitution process: institutions did not ban dueling (though many
 *   prosecuted it); they outcompeted it by offering better coordination for
 *   the disputes dueling had mediated. Honor-culture elites paid a cost
 *   (institutional friction, loss of authority over their own disputes) but
 *   gained systematic remedy and enforceability. No victim class emerges
 *   because the substitution was uncoerced — dueling remained legally and
 *   socially available in institutional gaps; its disappearance from use
 *   reflects preference for alternatives, not suppression. This reading
 *   contrasts with the contraction_reading (cultural shift in what dignity
 *   means) and the composite_reading (multiple independent sufficient
 *   causes). The institutional displacement reading claims ε is
 *   low-to-moderate: dueling benefits the institutions that displace it (low
 *   extraction from formal elites) but imposes no mandate and meets little
 *   resistance because alternatives are demonstrably superior for
 *   coordination.
 *
 * KEY AGENTS:
 *   - formal_legal_institutions: coordinate dispute resolution through courts; agenda-setter role
 *   - commercial_banking_system: create demand for predictable contract enforcement; institutional beneficiary
 *   - libel_law_framework: absorb insult-disputes through civil remedy; institutional beneficiary
 *   - honor_culture_elites: face institutional friction and loss of authority; payer role, constrained exit
 *   - emerging_commercial_class: benefit directly from institutional alternatives; mobile, organized beneficiary
 *   - rural_frontier_communities: retain dueling in institutional gaps; excluded from institutional design but retain practical availability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.28).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling as Institutionally-Displaced Dispute Resolution Protocol").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "legal_history/institutional_anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, '58f7c641-922f-497d-9e35-5c0795945a69').
narrative_ontology:cs_kernel_codification('58f7c641-922f-497d-9e35-5c0795945a69', distributed).
narrative_ontology:cs_authority_grounding('58f7c641-922f-497d-9e35-5c0795945a69', distributed).
narrative_ontology:cs_reading_relation('58f7c641-922f-497d-9e35-5c0795945a69', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('58f7c641-922f-497d-9e35-5c0795945a69', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('58f7c641-922f-497d-9e35-5c0795945a69', foundational, institutional_substitution_primary_mechanism).
narrative_ontology:cs_axiom_status(institutional_substitution_primary_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('58f7c641-922f-497d-9e35-5c0795945a69', institutional_substitution_primary_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('58f7c641-922f-497d-9e35-5c0795945a69', secondary, voluntary_institutional_adoption).
narrative_ontology:cs_axiom_status(voluntary_institutional_adoption, holdable).
narrative_ontology:cs_axiom_grounding('58f7c641-922f-497d-9e35-5c0795945a69', voluntary_institutional_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('58f7c641-922f-497d-9e35-5c0795945a69', dueling_as_legitimate_dispute_mechanism_in_institutional_gaps).
narrative_ontology:cs_drift_state('58f7c641-922f-497d-9e35-5c0795945a69', early_twentieth_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('58f7c641-922f-497d-9e35-5c0795945a69', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, formal_legal_institutions).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, commercial_banking_system).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_elites).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, emerging_commercial_class).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_elites).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__institutional_displacement_reading, slave_holding_south).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, judges, and legal procedure gradually absorb the dispute-resolution function dueling performed. They provide binding adjudication, enforceable remedy, and systematic record-keeping. They set the terms of acceptable dispute-resolution and accumulate legitimacy through consistent application. No deliberate suppression of dueling required — the institutional alternative simply works better for most classes of dispute.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, formal_legal_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Requires predictable, non-violent dispute settlement for credit and commercial contract enforcement. Dueling-based honor resolution is incompatible with debt collection, breach-of-contract disputes, and financial reputation systems. Banking's expansion creates structural demand for courts over duals; banking institutions benefit from the displacement without needing to enforce it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, commercial_banking_system, beneficiary,
    institutional, generational, analytical, national).

% Codifies reputational harm as a legal cause of action, providing non-violent remedy for slander and libel. Absorbs the 'insult requiring response' disputes that dueling historically mediated. Newspaper expansion and print libel create institutional demand for civil remedy over trial-by-combat.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, libel_law_framework, beneficiary,
    institutional, generational, analytical, national).

% Bear the immediate cost: they face pressure from institutional alternatives that compete with dueling as dispute-settlement. Dueling remains available (legally prosecuted but socially understood) in institutional gaps, but each generation has more incentive to use courts, banking contracts, and libel suits instead. The cost is not enforcement but institutional friction — they must participate in legal procedures they view as inferior, or accept the reputational hit of avoiding formal law.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_elites, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__institutional_displacement_reading, honor_culture_elites, beneficiary).

% Lacks the cultural investment in honor-mediation and benefits directly from courts and banking law. They accumulate wealth and political influence, reinforcing institutional alternatives to dueling through sheer economic weight and scale. Their preference is not imposed but is structurally rewarded by institutional design.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, emerging_commercial_class, beneficiary,
    organized, generational, mobile, national).

% Lack ready access to formal legal institutions; dueling persists as a available dispute mechanism in institutional gaps where courts are distant or absent. They are not consulted in the institutional design that displaces dueling; their exclusion from institutional infrastructure is the same mechanism that leaves dueling viable in their context.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, rural_frontier_communities, excluded,
    moderate, biographical, trapped, local).

% Culturally invested in honor-based dispute resolution and resists institutional displacement longer than the North. Court systems coexist with dueling longer; the institutional substitution is slower and contested. Civil War disruption accelerates displacement by shattering the social stability dueling depended on, but that is overdetermined causation (belongs to the composite_reading, not this one).
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, slave_holding_south, payer,
    powerful, biographical, constrained, regional).

% Tracks the institutional displacement mechanism as it unfolds: documents which disputes move to courts first, which lag, which institutional innovations most directly substitute for specific classes of duel (libel law for insult, commercial courts for property/debt, professional licensing for craft/trade honor). Records the pace and geography of substitution.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes dueling as the authoritative mechanism for resolving disputes over honor, insult, property claim, and contract breach — where institutional alternatives are absent or unavailable. Coordinates expectations: a gentleman knows his remedy is satisfaction by duel; a creditor knows the debtor knows courts exist; an insulted party knows public reputation requires acknowledgment (duel or legal suit). The coordination function is 'what counts as legitimate dispute settlement in this context.'
% TRANSFER_FUNCTION: Transfers reputational validation and social standing from the duelist who maintains honor through the duel to institutional systems that provide binding resolution. The constraint channels disputes from honor-mediated settlement (winner takes social validation) into institutional channels (winner takes legal judgment + enforcement). The beneficiaries are the institutional alternatives that absorb dispute volume.
% ABSENT_VOICES: Rural communities with limited institutional access; enslaved persons with no access to any legitimate dispute mechanism; women excluded from dueling entirely. The institutional displacement assumes access to courts and commercial systems — populations without such access are forced into either dueling (continued high-violence mechanism) or unresolved grievance.
% DISAPPEARANCE_RATIONALE: If dueling as an available mechanism vanished (institutional substitution completed), disputes would continue but would be resolved exclusively through law, commercial arbitration, and social exclusion of non-institutional resolvents. The world rearranges because dispute-settlement is essential to social functioning; removing one mechanism forces use of others (or forces festering). If institutional alternatives vanished (law, banking, libel suit), dueling would re-emerge as the only available high-stakes remedy.
% FOUNDING_PROBLEM: Disputes over honor, insult, property, and contract required settlement mechanisms. Dueling provided a binding, conclusive, high-stakes settlement in pre-institutional contexts where courts were weak, inaccessible, or lacked legitimacy to adjudicate honor. The founding problem is coordination: how do aggrieved parties know their remedy is legitimate, and how do others know the remedy has been performed?
% FOUNDING_PROBLEM_CORROBORATION: Historians attest the founding problem was live in 16th-18th century honor cultures (weakness of court access and legitimacy in rural areas, foreign gentlemen, matters of personal reputation beyond property). Commercial institutions and legal scholars attest the problem is substantially solved by courts and banking systems in the 19th century (court accessibility increased, commercial law developed, libel law codified). The reading's core claim (institutional substitution) is corroborated by comparative analysis of dispute type over time: property disputes move to courts first, insult disputes follow as libel law develops, credit disputes move as banking law standardizes.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (~0.15 in 1500) because dueling is the dominant mechanism — no institution yet exists to extract from its displacement. It rises gradually (0.22 by 1700) as institutional alternatives develop and demand their use, peaking at 1850 (~0.32) when courts, banking law, and libel law are fully developed but honor culture still exerts pressure to use dueling. It then stabilizes or slightly declines by 1900 as institutional dominance becomes uncontested — the pressure to use dueling drops because the institutional alternative is so obviously superior. Theater ratio is consistently low (never above 0.12) because the institutional substitution is real: courts do actually settle disputes, banking law does enforce contracts, libel suits do provide remedy. There is no theatrical maintenance of dueling, merely its persistence in institutional gaps (rural courts, frontier settlements, military officer corps). Suppression is deliberately kept low (0.05–0.13) because this reading does not emphasize legal prohibition or enforcement against dueling — that belongs to the composite reading. Institutional competition alone explains the shift; no special coercion is needed. The measurement series are aligned on one shared time grid (every metric authored at every time point), and the trajectory shows the classic institutional displacement curve: emergence of alternatives → gradual shift in practice → eventual dominance of institutional mechanism → residual dueling in gaps.
 *
 * PERSPECTIVAL GAP:
 *   The formal institutional seat (courts, banking, law) would experience this constraint as coordination on their own expansion and legitimacy — they compute as beneficiaries with no extraction cost. The honor-culture elite seat would experience it as institutional friction and loss of private authority — they compute as payers bearing cost but not victims (because the institutional alternative is genuinely superior and voluntary). Rural frontier communities would experience it as institutional gaps where dueling remains available but institutionally disfavored — they compute as moderately constrained but not suppressed. The engine should reflect these structural differences in per-seat computation; this story's claim (rope) rests on the voluntary nature of the substitution and the absence of a victim class at the story level.
 *
 * DIRECTIONALITY LOGIC:
 *   Dueling itself is not extractive in this reading; it is a coordination mechanism that benefits those who use it (honor-culture elites) and persists where it works (institutional gaps). The constraint at the story level is 'dueling as institutionally-displaced dispute mechanism,' which captures the relationship between dueling and the institutional alternatives. From the perspective of formal legal institutions, commercial banking, and libel law, the constraint coordinates their expansion and legitimacy — they benefit structurally from dueling's fringe status as it validates their necessity. From the perspective of honor-culture elites, the constraint imposes cost (institutional friction, loss of private authority) but no suppression. The substitution is attractive despite the cost because institutions deliver better coordination. This is why the claimed type is rope: it solves a real coordination problem (how do we settle disputes now that dueling is less available?) without requiring coercion. No victim class exists because exit from dueling to institutions is mobile and voluntary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live in 1500–1700 (weak institutional dispute-resolution exists and dueling fills the gap). By 1900, institutional alternatives solve the dispute-resolution function so thoroughly that dueling's founding problem is dead — the mechanism persists in institutional gaps (where coordination still requires it) and in cultural memory but no longer mediates a live coordination problem for most of society. This creates mandatrophy: the constraint persists not because the founding problem is live but because institutional momentum and cultural practice sustain it. However, the institutional-displacement reading does not emphasize mandatrophy as the PRIMARY mechanism of persistence — it emphasizes institutional competition. Mandatrophy would be the subject of the piton reading (if one were authored). This reading stays rope because the constraint still coordinates something: it marks the boundaries of institutional legitimacy, shows which disputes have been absorbed into law, and operates as the residual mechanism in institutional gaps. The constraint's persistence is explained by its continued utility in those gaps, not by institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_substitution_vs_cultural_displacement,
    'Is dueling''s decline primarily driven by institutional substitution (courts outcompeting it) or cultural displacement (honor-culture yielding to dignity-culture)? These are different mechanisms producing the same terminal state.',
    'Geographic and temporal analysis of institutional development vs. cultural change: do disputes move to courts BEFORE cultural attitudes toward honor shift, or after? Which predicts the shift better — court availability or intellectual/religious discourse change? Case studies of regions with early court development but persistent honor-culture; regions with cultural shift but slow institutional development.',
    'If institutional substitution drives decline: the constraint is rope, dueling is displaced by better alternatives, no victim class. If cultural displacement drives decline: the constraint may be tangled_rope (honor-culture elites are victims of culture change imposed through education, religion, print), type changes to snare if displacement is enforced through stigmatization. Ε changes significantly depending on which mechanism is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_substitution_vs_cultural_displacement, empirical, 'Whether institutional or cultural mechanism is primary driver of dueling''s decline.').

omega_variable(
    institutional_gaps_as_evidence,
    'Does dueling''s persistence in institutional gaps (rural courts, frontier, military) support the institutional-displacement reading, or does it reflect cultural heterogeneity that contradicts institutional substitution?',
    'Track what disputes persist as dueling in institutional gaps: are they disputes courts explicitly handle poorly or avoid? (supports displacement) Or are they disputes where honor-culture is stronger regardless of institutional availability? (supports cultural mechanism). Analyze whether gap-populations adopt institutional alternatives quickly when access improves or resist them culturally.',
    'If gaps reflect institutional absence: displacement reading is strengthened, ε stays low (~0.28). If gaps reflect cultural preference despite institutional availability: the reading is weakened, ε should be higher, and cultural displacement becomes more plausible as primary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gaps_as_evidence, empirical, 'Whether institutional gaps show institutional displacement mechanism is operating.').

omega_variable(
    reading_boundary_vs_composite_causation,
    'Is the institutional-displacement mechanism sufficient to explain dueling''s decline, or does explaining the full decline require the composite reading (multiple independent sufficient causes: legal prohibition, institutional modernization, cultural shift, Civil War trauma)?',
    'Counterfactual analysis: if only institutional alternatives had developed but legal prohibition and Civil War had not occurred, would dueling have declined as far? If only cultural shift had occurred but institutions had not modernized, would dueling have declined as far? Comparative historical cases (Europe vs. South America; antebellum vs. postbellum US).',
    'If institutional displacement is sufficient: this reading is complete, ε is well-grounded, the reading forecloses the composite reading''s necessity claim. If multiple mechanisms are necessary: the reading is incomplete, composite reading is more accurate, and the dueling constraint is overdetermined (belongs in composite story, not here). This omega determines whether this reading stands alone or merges into composite.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_vs_composite_causation, conceptual, 'Whether institutional displacement is sufficient cause or whether multiple mechanisms are necessary.').

omega_variable(
    honor_elite_voluntariness,
    'Did honor-culture elites genuinely choose institutional alternatives voluntarily (rope reading: cost-benefit rational), or were they gradually pressured/forced into institutional compliance (tangled_rope reading: coerced participation despite cultural preference)?',
    'Documentary evidence of elite discourse: do they frame institutional alternatives as superior solutions they choose, or as impositions they resent? Track elite litigation rate and dueling rate together — do elites litigate while dueling, or do they switch? Economic data: do elites benefit from institutional mechanisms (e.g., enforced debt collection through courts) in ways that make institutional participation rational? Social data: are elites who refuse courts excluded or merely stigmatized?',
    'If genuinely voluntary: rope reading holds, no victim class, ε ~0.28. If coerced through institutional pressure: reading shifts toward tangled_rope or snare, honor elites are victims, ε rises significantly, suppression rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_elite_voluntariness, empirical, 'Whether elites'' institutional participation is voluntary or coerced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1500, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1500, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1500, 0.02).
narrative_ontology:measurement(duel_tr_t1600, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1600, 0.03).
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.08).

% Extraction over time
narrative_ontology:measurement(duel_be_t1500, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(duel_be_t1600, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1600, 0.18).
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1700, 0.22).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1800, 0.28).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1850, 0.32).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1500, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(duel_su_t1600, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1600, 0.06).
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1700, 0.08).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1850, 0.13).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__institutional_displacement_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'dueling_disappearance_mechanism.' The institutional-displacement reading emphasizes institutional competition and substitution; the contraction reading emphasizes cultural shift in what dignity means; the composite reading identifies multiple independent sufficient causes. Each reading instantiates a different constraint with a potentially different ε and type, sharing the same historical referent (dueling's 16th–19th century decline). The network links capture the dependency: institutional development creates conditions for but does not determine cultural shift; cultural shift creates demand for but does not require institutional development; both contribute to composite causation but neither fully explains it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
