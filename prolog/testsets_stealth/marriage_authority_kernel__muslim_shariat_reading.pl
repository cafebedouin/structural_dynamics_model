% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Shariat-Based Marriage and Family Law Authority (Muslim Personal Law Reading)
 *   domain: legal/religious/constitutional-pluralism
 *
 * SUMMARY:
 *   In India's plural personal-law order, marriage, divorce, maintenance, and
 *   inheritance for Muslims are governed by Shariat as interpreted by
 *   community institutions: the All India Muslim Personal Law Board,
 *   affiliated darul iftas, and practicing qazis. The arrangement delivers
 *   real services (ritual validity, dispute resolution, identity continuity)
 *   while concentrating asymmetric burdens on women: unilateral talaq
 *   (partially curtailed since 2017), polygamy, and unequal inheritance. KEY
 *   AGENTS (by structural relationship): all_india_personal_law_board: agenda
 *   setter (institutional/identity_locked) — administers interpretation and
 *   defends jurisdiction; qazis_and_darul_ifta_muftis: fee-and-status
 *   collectors who also run day-to-day adjudication
 *   (organized/identity_locked); male_kin_under_nikah_regime: primary
 *   material beneficiary (moderate/constrained);
 *   muslim_women_under_personal_law: primary bearer of the asymmetric burdens
 *   (moderate/constrained, court backstop);
 *   reformist_muslim_womens_organizations: excluded voice pressing from
 *   outside the interpretive hierarchy; constitutional_courts: analytical
 *   observer that prunes practices without replacing the framework;
 *   sma_exit_couples: arbitrage-grade exit channel. This story instantiates
 *   ONE reading of the marriage_authority_kernel; sibling readings are
 *   separate constraints in separate files. The epsilon referent is the
 *   standing Shariat-based arrangement as it actually operates, assessed
 *   including internal reformist critique (Shayara Bano petitioners, BMMA) —
 *   not the arrangement as the boards describe it, and not any sibling
 *   reading's endorsed alternative.
 *
 * KEY AGENTS:
 *   - all_india_muslim_personal_law_board: Agenda setter (institutional/identity_locked) — controls interpretation, defends community jurisdiction, absorbs the political cost of defense
 *   - qazis_and_darul_ifta_muftis: Secondary agenda setter and collector (organized/identity_locked) — fees, offerings, and status flow through adjudication roles
 *   - male_kin_under_nikah_regime: Primary material beneficiary (moderate/constrained) — holds divorce initiative, plural-marriage option, larger inheritance shares
 *   - muslim_women_under_personal_law: Primary payer (moderate/constrained) — bears divorce insecurity, polygamy exposure, inheritance disadvantage; constitutional recourse exists at high social cost
 *   - reformist_muslim_womens_organizations: Excluded voice (organized/mobile) — organizes litigation and reform campaigns from outside the interpretive bodies
 *   - constitutional_courts: Analytical observer (institutional/analytical) — prunes specific practices, declines wholesale replacement
 *   - sma_exit_couples: Arbitrage exit demonstration (moderate/arbitrage) — civil-marriage opt-out channel that damps effective reach
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.62).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Shariat-Based Marriage and Family Law Authority (Muslim Personal Law Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "legal/religious/constitutional-pluralism").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '8d98ef09-d32a-4226-9fd8-17f99af9fbaf').
narrative_ontology:cs_kernel_codification('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', fixed_text).
narrative_ontology:cs_authority_grounding('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', lineage).
narrative_ontology:cs_interpretation_layer_present('8d98ef09-d32a-4226-9fd8-17f99af9fbaf').
narrative_ontology:cs_reading_relation('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', foundational, divine_law_not_legislative_object).
narrative_ontology:cs_axiom_status(divine_law_not_legislative_object, holdable).
narrative_ontology:cs_axiom_grounding('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', divine_law_not_legislative_object, theological).
narrative_ontology:cs_axiom('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', secondary, qualified_interpreter_transmission_authority).
narrative_ontology:cs_axiom_status(qualified_interpreter_transmission_authority, holdable).
narrative_ontology:cs_axiom_grounding('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', qualified_interpreter_transmission_authority, theological).
narrative_ontology:cs_reference_frame('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', immutable_divine_revelation_framework).
narrative_ontology:cs_drift_state('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', contemporary_ucc_debate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8d98ef09-d32a-4226-9fd8-17f99af9fbaf', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, all_india_muslim_personal_law_board).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis_and_darul_ifta_muftis).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, male_kin_under_nikah_regime).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_women_under_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, sma_exit_couples).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__muslim_shariat_reading, personal_law_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues interpretive pronouncements on nikah, talaq, khula, maintenance, and inheritance; lobbies legislatures and courts to keep family-law jurisdiction inside community institutions; coordinates affiliated darul iftas and qazis. Its standing, funding, and political weight depend on being recognized as the authoritative voice of Shariat application; relinquishing that role would leave the organization without a function.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, all_india_muslim_personal_law_board, agenda_setter,
    institutional, generational, identity_locked, national).

% Solemnize marriages, register divorces, answer fiqh queries, and mediate family disputes for fees, offerings, and communal status. Their livelihood and social rank flow from serving as intermediaries between households and the tradition; madrasa training pipelines bind entire careers to the continuation of the role.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis_and_darul_ifta_muftis, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, qazis_and_darul_ifta_muftis, agenda_setter).

% Married men under the regime hold unilateral divorce initiation (talaq), the option of plural marriage, and larger statutory inheritance shares; they owe mehr and maintenance in return. Opting into the civil-marriage track is legally available but typically costs family approval and community standing, so most remain inside the arrangement for life.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, male_kin_under_nikah_regime, beneficiary,
    moderate, biographical, constrained, national).

% Married women under the regime face divorce initiated without their consent, exposure to a co-wife, and smaller inheritance shares; they hold khula (a consensual exit that usually returns the mehr) and access to constitutional courts, as the Shayara Bano petitioners demonstrated. Using those channels means years of litigation and often estrangement from natal and marital family support networks.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_women_under_personal_law, payer,
    moderate, biographical, constrained, national).

% Organize inside the community for codified, gender-equal family law: drafting model nikahnamas, petitioning courts, campaigning for legislative reform. They are consulted late or symbolically by the boards and hold no vote in interpretive bodies; their leverage runs through courts and legislatures rather than through the interpretive hierarchy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, reformist_muslim_womens_organizations, excluded,
    organized, generational, mobile, national).

% Adjudicate challenges that personal-law practices violate fundamental rights; struck down talaq-e-biddat in Shayara Bano (2017) while expressly leaving the wider personal-law framework intact. They can prune specific practices but have declined to rewrite the framework wholesale, deferring that to legislation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Couples who marry under the Special Marriage Act instead of nikah place their marriage, divorce, and succession under civil law. The exit is legally open and increasingly used, but it typically requires notice periods, family opposition, and, for some, loss of community recognition, so it draws a self-selected minority rather than the whole population.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, sma_exit_couples, payer,
    moderate, biographical, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, male_kin_under_nikah_regime).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified, community-administered framework for forming and dissolving marriages, settling maintenance, and dividing inheritance among Muslims within a plural legal order: ritual validity, legitimacy, and dispute resolution supplied without reliance on state courts.
% TRANSFER_FUNCTION: Moves adjudicative authority, fees, and deference from households to the interpreter class (boards, qazis, muftis); moves divorce initiative, remarriage latitude, and inheritance share from wives and daughters to husbands and sons.
% ABSENT_VOICES: Reformist Muslim women's organizations and ordinary women party to disputes were historically absent from board deliberations; constitutional jurists sit entirely outside the interpretive hierarchy. They object from courts, legislatures, and civil society rather than from seats in the interpretive bodies.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand the legitimacy of millions of existing nikahs, push every maintenance and inheritance dispute into civil courts, sever a primary identity institution for the community, and trigger immediate political realignment; the surrounding plural order is arranged around this authority existing.
% FOUNDING_PROBLEM: Colonial Anglo-Muhammadan law applied Shariat erratically through judge-made precedent; the movement behind the 1937 Shariat Application Act sought uniform application of Muslim family law and protection of community legal autonomy from state and judicial assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the 1937 legislative debates (sponsored by Muslim reformist legislators, recorded in colonial legislative proceedings) and by constitutional-court opinions acknowledging the autonomy interest while curbing specific practices (Shayara Bano, 2017). Internal reformist women's groups attest that the protective rationale has partially decayed into office-preservation for the interpreter class; the boards themselves deny any decay.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement delivers genuine services (solemnization, mediation, inheritance settlement) while the gender-asymmetric bundle (talaq initiative, polygamy, inheritance shares) transfers durable value from women to male kin; the 2017-2019 removal of talaq-e-biddat trimmed one mechanism without touching the rest. Suppression 0.62: enforcement is predominantly social and informal (family pressure, community sanction, economic dependency) plus periodic legislative entrenchment (the 1986 Act reversing Shah Bano); after criminalization of instant talaq, enforcement leaned harder on internal channels, raising the suppressive load. Theater 0.42: a growing share of board activity is positional politics (fatwas, press statements, UCC opposition) whose adjudicative effect is small, while actual dispute resolution migrates to civil courts and informal family settlement; the political function remains live, so this is partial, not total, performance. Accessibility_collapse 0.45: alternatives (Special Marriage Act, khula, constitutional litigation) genuinely exist but carry heavy social cost, so alternatives persist rather than collapsing. Resistance 0.6: sustained internal litigation and organizing meet the arrangement continuously. Claimed type tangled_rope is authored independently of these scores: the structure possesses a real coordination function, identifiable payers bearing asymmetric burdens through the same structure, and active enforcement — all three canonical requirements. The measurement series run on one shared grid (0, 15, 30, 45, 60, 75, 88; interval start approximates the 1937 Shariat Application Act, end the present). Inflections: the 1986 Muslim Women Act ratchet sits between grid points 45 and 60 (captured as the 0.57-to-0.63 rise); the Shayara Bano / 2019 criminalization step-down sits between 75 and 88 (0.61-to-0.58 fall). Suppression_requirement is tracked because enforcement capacity is the dynamic this story traces: a ratchet upward at state entrenchment, then a shift from formal to informal enforcement after criminalization.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently from identical structural data. From muslim_women_under_personal_law, the arrangement operates as enforced asymmetry with costly exits; from male_kin_under_nikah_regime, the same rules read as sacred obligation balanced by mehr and maintenance duties; from the board and qazi seats, the framework is custodianship of a divine order under siege, and every court pruning reads as assault rather than correction. Same community, same texts, divergent computed classifications — the engine derives this divergence from the declared positions; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The three declared beneficiary groups derive low directionality: the board (agenda setter, identity-locked) and qazis (fee collectors, identity-locked) sit near the subsidy end because the arrangement constitutes their authority and livelihood; male kin derive low-but-not-zero d because they bear real counter-duties (mehr, maintenance) while holding the decisive advantages. Muslim women derive high d near the full-target end: they bear the asymmetric burdens and their exit is constrained (courts exist but cost years and kinship networks), placing them nearer the trapped end than mobile actors. Sma_exit_couples carry arbitrage-grade exit, damping their effective position. Constitutional courts are analytical observers contributing no extraction. Scope is national, which modestly amplifies effective extraction through verification difficulty across a vast, heterogeneous population.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (shielding community family law from erratic colonial judge-made application and later assimilation) retains a live core: pluralism genuinely protects minority identity in a majoritarian polity. But the specific protections have partially decayed into authority-preservation for the interpreter class, visible in the rising theater ratio and in the boards' resistance to reforms their own constituents litigate for. Mandatrophy is therefore NOT resolved: the adjudicative mandate still functions for a substantial population even as its performative share grows. The tangled_rope classification prevents both mislabels: a snare label would erase the coordination millions genuinely rely on for marriage legitimacy and estate settlement; a rope label would erase the documented asymmetric burden the manifest itself flags as this reading's structural delta. Under the mismatch consumer, founding_problem_status 'contested' paired with disappearance_verdict 'world_rearranges' asserts no zombie flag — the parties genuinely dispute obsolescence rather than the arrangement persisting past a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of marriage_authority_kernel; what would each sibling reading change structurally if adopted?',
    'Compile the four sibling stories and compare victim/beneficiary sets, epsilon values, and per-seat classifications across the kernel family.',
    'Adopting secular_civil_reading relocates authority to individual consent and dissolves this reading''s interpreter-class beneficiaries entirely; adopting hindu_codified_reading swaps the asymmetry profile (no unilateral extra-judicial divorce, but different maintenance and coparcenary asymmetries) without eliminating gender asymmetry; the christian and parsi readings carry smaller populations and weaker enforcement machinery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    divine_origin_vs_constructed_institution,
    'Is the immutability claim (Shariat as divine and beyond legislative reach) a genuine limit on the arrangement, or a constructed authority-preserving claim benefiting the interpreter class?',
    'Comparative fiqh history: the content claimed as immutable varies across schools and eras; jurisdictions such as Turkey (1926) and Tunisia (Majalla reforms) revised family law within Muslim majorities, showing the ''immutable'' package is historically variable.',
    'If constructed, the naturality claim collapses and the arrangement stands as an enforced human institution with identifiable collectors; if practitioners experience it as genuine sacred obligation, part of the measured suppression is devotion rather than coercion, and effective extraction drops accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_origin_vs_constructed_institution, conceptual, 'Whether the immutability framing is natural law or institutional self-protection.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression sustaining women''s compliance structural (economic dependency, natal-family pressure, custody fear) or internalized (piety identification with the framework, belief that the asymmetry is divinely ordered)?',
    'Post-exit trajectory study of women who exit via khula or Special Marriage Act: if perceived obligation and self-censorship persist after the structural mechanism is removed, a substantial internalized share exists.',
    'If largely internalized, effective suppression exceeds the structural measure and survives formal reform; if largely structural, legislative and economic remedies would release compliance quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    enforcement_displacement_after_criminalization,
    'Did the 2017 Shayara Bano judgment and the 2019 criminalization of talaq-e-biddat reduce the arrangement''s effective extraction, or merely displace enforcement into informal channels (unwitnessed oral divorce, community pressure, migration of practice to states with weak registration)?',
    'Divorce-registration data and incidence surveys comparing talaq rates and dispute venues before and after 2017-2019.',
    'If displacement dominates, the post-2017 extractiveness decline in the measurement series is a measurement artifact and the true trajectory stays flat or rises; if incidence genuinely fell, the decline is real and further codification would compound it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_displacement_after_criminalization, empirical, 'Whether criminalization reduced extraction or displaced it informally.').

omega_variable(
    board_representativeness_theater,
    'What share of the community actually adjudicates marriage disputes through boards and qazis, versus civil courts and informal family settlement?',
    'Survey data on dispute-resolution venue selection among Muslim households, cross-checked against darul ifta caseload records and court statistics.',
    'A high informal-settlement share would raise the theater ratio further and weaken the enforcement gate, drifting the adjudicative function toward inertial maintenance while the political function stays live; a high board-utilization share would confirm the coordination function as primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(board_representativeness_theater, empirical, 'Whether claimed jurisdiction matches actual usage.').

omega_variable(
    ucc_replacement_trajectory,
    'Will the uniform-civil-code trajectory (Uttarakhand 2024, national debate) replace this arrangement wholesale, and would replacement resolve or merely relocate the gender asymmetry?',
    'Legislative outcomes and post-adoption litigation patterns in states adopting a civil code.',
    'Wholesale adoption would convert this constraint toward a transitional remnant with a de facto sunset; the equity outcome depends entirely on the code''s content, which the parties dispute — replacement is not automatically de-extracting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ucc_replacement_trajectory, preference, 'Policy-contingent future of the arrangement.').

omega_variable(
    interpreter_class_framing,
    'Is the operative kernel the revealed texts themselves (framing chosen here: fixed_text, lineage authority), or the custodial authority of the interpreter class layered above the texts?',
    'Observe where interpretive disputes actually terminate: if appeals run to textual argument, the texts are the kernel; if they terminate in board prerogative and institutional standing, the interpreter class is the kernel.',
    'Under the alternative framing, authority_grounding shifts toward extraction (the boards benefit from preventing kernel revision), the theater ratio weights more heavily, and the drift computation would read the same court interventions as direct kernel challenge rather than interpretive-layer absorption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpreter_class_framing, conceptual, 'Framing under-determination in the commitment-system classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 0, 88).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(marr_tr_t15, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(marr_tr_t45, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(marr_tr_t75, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 75, 0.38).
narrative_ontology:measurement(marr_tr_t88, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 88, 0.42).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(marr_be_t15, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(marr_be_t45, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 45, 0.57).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(marr_be_t75, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 75, 0.61).
narrative_ontology:measurement(marr_be_t88, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 88, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(marr_su_t15, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 15, 0.37).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(marr_su_t45, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 45, 0.46).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(marr_su_t75, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 75, 0.58).
narrative_ontology:measurement(marr_su_t88, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 88, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'marriage/family law authority in India' decomposes into five structurally distinct constraints, one per reading of the marriage_authority_kernel, each with its own epsilon, beneficiary/victim structure, and classification. This file instantiates the muslim_shariat_reading (community-interpreted divine law; highest enforcement informality, strongest identity-lock, gender-asymmetry bundle flagged in the manifest delta). The upstream/downstream structure is asymmetric: the hindu_codified_reading was reformed by statute in 1955-56 partly because this reading was left unreformed, and this reading's political mobilization (the 1986 Shah Bano reversal, minority-autonomy jurisprudence) materially constrains the secular_civil_reading's adoption conditions — hence the influences edge toward the secular sibling. All family members are linked via network.affects_constraints; no member is an orphan.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
