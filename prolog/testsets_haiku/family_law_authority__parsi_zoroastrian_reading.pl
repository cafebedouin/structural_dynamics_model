% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage Law: Community Preservation via Religious Authority
 *   domain: religious_law/family/political
 *
 * SUMMARY:
 *   Marriage in the Parsi Zoroastrian tradition operates as a
 *   community-preserving institution governed by religious law and priestly
 *   authority. The constraint enforces endogamy (marriage within the faith)
 *   and gates conversion, historically justified as necessary to preserve a
 *   small diaspora against dissolution. This reading instantiates the
 *   Zoroastrian framework: marriage validity depends on priestly recognition,
 *   authority flows through a lineage of religious specialists, and the
 *   constraint's legitimacy rests on the premise that small communities
 *   require boundary maintenance to survive. The claim is tangled_rope
 *   (genuine coordination function — binding community members through shared
 *   ritual and kinship — AND asymmetric extraction from those seeking
 *   intermarriage or conversion). The metrics model the actual operation:
 *   extractiveness has risen modestly over the interval as demographic
 *   pressure increases and reform pressure mounts, forcing the priesthood to
 *   defend the rule more actively; suppression similarly increases as exit
 *   becomes more visible and resistance grows from reform movements and
 *   younger diaspora members; theater rises as the coordination justification
 *   becomes less salient relative to boundary-maintenance performance.
 *
 * KEY AGENTS:
 *   - Zoroastrian priesthood: gate-keeper of ritual validity; derives authority and standing from endogamy enforcement
 *   - Parsi community leadership: invokes endogamy as preservation mechanism; frames boundary maintenance as survival necessity
 *   - Individuals choosing intermarriage: bear identity-lock suppression; exit means family and community dissolution
 *   - Non-Parsi spouses: excluded from ritual and community standing; bear costs of partner's affiliation
 *   - Converts and would-be converts: locked out by priesthood gatekeeping; institutionally prevented from entry
 *   - Reform clergy: excluded from orthodox authority structures; advocate relaxed conversion and intermarriage rules
 *   - Civil authorities: observe and adjudicate boundary cases; increasingly check religious law against civil rights guarantees
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.68).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage Law: Community Preservation via Religious Authority").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious_law/family/political").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, 'd5c593d5-5035-4666-8af9-4e44254dcdd1').
narrative_ontology:cs_kernel_codification('d5c593d5-5035-4666-8af9-4e44254dcdd1', fixed_text).
narrative_ontology:cs_authority_grounding('d5c593d5-5035-4666-8af9-4e44254dcdd1', lineage).
narrative_ontology:cs_interpretation_layer_present('d5c593d5-5035-4666-8af9-4e44254dcdd1').
narrative_ontology:cs_reading_relation('d5c593d5-5035-4666-8af9-4e44254dcdd1', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5c593d5-5035-4666-8af9-4e44254dcdd1', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5c593d5-5035-4666-8af9-4e44254dcdd1', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5c593d5-5035-4666-8af9-4e44254dcdd1', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('d5c593d5-5035-4666-8af9-4e44254dcdd1', foundational, endogamy_necessary_for_community_survival).
narrative_ontology:cs_axiom_status(endogamy_necessary_for_community_survival, holdable).
narrative_ontology:cs_axiom_grounding('d5c593d5-5035-4666-8af9-4e44254dcdd1', endogamy_necessary_for_community_survival, empirically_contingent).
narrative_ontology:cs_axiom('d5c593d5-5035-4666-8af9-4e44254dcdd1', foundational, priestly_ritual_authority_grounds_marriage_legitimacy).
narrative_ontology:cs_axiom_status(priestly_ritual_authority_grounds_marriage_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d5c593d5-5035-4666-8af9-4e44254dcdd1', priestly_ritual_authority_grounds_marriage_legitimacy, theological).
narrative_ontology:cs_reference_frame('d5c593d5-5035-4666-8af9-4e44254dcdd1', priestly_authority_over_ritual_validity).
narrative_ontology:cs_drift_state('d5c593d5-5035-4666-8af9-4e44254dcdd1', contemporary_diaspora_reform_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d5c593d5-5035-4666-8af9-4e44254dcdd1', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_cohesion).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, individuals_choosing_intermarriage).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, converts_to_zoroastrianism).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_community_leadership).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers marriage ritual (yasna) and declares ritual validity; interprets Zoroastrian law through textual authority (Avesta, Pahlavi texts); controls who may marry within the faith community and enforces endogamy norms. Authority rests on claimed transmission of priestly knowledge and ritual competence. Collects legitimacy and social standing from gate-keeping role.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood, agenda_setter,
    organized, generational, mobile, regional).

% Community councils and elder bodies that invoke the endogamy rule to preserve Parsi identity and institutional continuity. Frame the constraint as necessary to prevent cultural dissolution in diaspora contexts. Derive authority and community cohesion from the arrangement.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_community_leadership, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_community_leadership, agenda_setter).

% Parsi individuals who wish to marry outside the faith face loss of religious status, community recognition, and family standing. The constraint operates through identity fusion: leaving the community or choosing a non-Parsi spouse means renunciation of Zoroastrian identity, which many experience as family dissolution rather than exit. Marriage outside the faith is technically permitted by civil law but religiously unrecognized.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, individuals_choosing_intermarriage, payer,
    moderate, biographical, identity_locked, regional).

% Non-Zoroastrians married to Parsis are excluded from priestly ritual participation and may be subject to community ostracism. They bear the costs of their partner's cultural affiliation without access to the community's institutional recognition or benefits. Their children may not be recognized as Parsis in traditional doctrine.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses, payer,
    moderate, biographical, constrained, regional).

% Formal conversion to Zoroastrianism is historically discouraged or prevented by priesthood gatekeeping. Converts face an institutional barrier: even those who undergo conversion rituals are often not accepted as fully Parsi within community structures, and their children remain outside the endogamous community. The constraint preserves the community by making entry functionally impossible.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, converts_to_zoroastrianism, payer,
    powerless, biographical, trapped, regional).

% Historically bear differential costs: in some lineages, children of a Parsi woman and non-Parsi man are excluded from community standing, while the reverse may be tolerated. Modern reform movements contest this, but traditional doctrine enforces stricter endogamy rules for women, binding them to in-group marriage through both identity and institutional rules.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_women, payer,
    moderate, biographical, identity_locked, regional).

% Progressive Zoroastrian clergy and theological movements that advocate relaxing or reinterpreting endogamy rules to allow conversion and intermarriage. Their voices are structurally marginalized in orthodox priesthood councils but carry weight in diaspora communities facing demographic decline. They are not at the table where the traditional endogamy rule is enforced.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, reform_clergy, excluded,
    organized, generational, mobile, regional).

% The principle that small diaspora communities require boundary maintenance to avoid dissolution. This logic is vindicated by the constraint's operation but is not itself an actor with interests; it is a strategic doctrine invoked to justify the arrangement.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, demographic_continuity_logic, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(family_law_authority__parsi_zoroastrian_reading, demographic_continuity_logic).

% State legal systems in India (for Parsis under Parsi Marriage and Divorce Act 1936), UK, and elsewhere recognize some aspects of Zoroastrian family law but increasingly adjudicate disputes over endogamy rules, convert status, and women's succession rights. They occupy an analytical seat, investigating whether the constraint violates civil law guarantees.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, civil_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains linguistic, ritual, and genealogical continuity within a diaspora community whose survival depends on in-group reproduction and marriage, preventing cultural dissolution through outmarriage. Operates a shared marriage ritual, inheritance doctrine, and kinship identity that bind community members.
% TRANSFER_FUNCTION: Transfers from individuals choosing intermarriage and would-be converts the privilege of full community membership, family standing, and recognized religious identity. Moves this privilege upward to the priesthood (who control ritual validity) and to community leadership (who maintain cohesion). In-group members benefit from the preserved community; out-group actors and those seeking entry bear the cost.
% ABSENT_VOICES: Reform clergy who would argue for conversion rights and intermarriage recognition; younger diaspora Parsis whose demographic context differs from the founding era; non-Parsi spouses and their extended families who experience the constraint but have no seat at community councils where marriage law is interpreted.
% DISAPPEARANCE_RATIONALE: If the endogamy requirement and priesthood gatekeeping disappeared, the Parsi community would face immediate demographic choice: openness to conversion and intermarriage would allow growth and integration but risk erosion of distinct identity; the constraint's removal would force explicit renegotiation of 'what it means to be Parsi' rather than settling it through ritual law. Community councils would lose their primary institution for boundary maintenance.
% FOUNDING_PROBLEM: Preservation of a small Zoroastrian diaspora (6,000–10,000 living Parsis globally) after the Islamic conquest of Persia and forced exile to India (8th–10th centuries). Endogamy and priesthood authority over marriage were survival mechanisms to prevent absorption into surrounding populations and preserve ritual knowledge across generations.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox priesthood attests the founding problem is ongoing — demographic decline and assimilation pressure in diaspora justify continued endogamy enforcement. Reform theologians and secular Parsi scholars attest the original survival problem is solved (no current existential threat of forced religious conversion) and the rule now functions as pure boundary maintenance. Demographers and religious historians from outside the Parsi community note that smaller diaspora religions (e.g., Yazidis, Mandaeans) survive with more permissive marriage rules, suggesting the founding problem does not require the constraint in its current form.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the constraint's core operation: it transfers community membership status from individuals who choose exit-paths (intermarriage, conversion) to the priesthood and community leadership who control the gate. The constraint is functional (genuine coordination: shared ritual, kinship bonds, identity continuity) but substantially extractive (those who wish to intermarry or convert lose standing). Suppression (0.72) is high because exit from the community is effectively foreclosed for identity-locked individuals — renouncing Parsi identity is experienced as family dissolution, not merely a religious choice. The constraint persists through active enforcement: priesthood gatekeeping is continuous, community councils reinforce norms, and ritual validity is withheld from non-sanctioned marriages. Theater_ratio (0.41) captures rising performative maintenance: as reform pressure grows and demographic decline becomes acute, proportionally more priesthood and community authority is devoted to *performing* (through speeches, ritual emphasis, community gatherings) the necessity of endogamy, rather than its actual enforcement. The plateau at t30–t40 reflects stabilization: the constraint has reached a steady state where resistance and reform pressure are high but gatekeeping remains stable, suggesting the system is no longer extracting efficiently and is increasingly theatrical. The time grid is uniform across all three metrics: every metric is authored at every time point (0, 5, 10, 15, 20, 25, 30, 40) to avoid misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The Parsi priesthood and community leadership see a constraint that solves a real coordination problem: maintaining shared identity, ritual knowledge, and community continuity in diaspora. From their seat, the constraint is Rope-like. But from the seat of an individual with identity-lock (Parsi identity is foundational to family, self-concept, community belonging), the same constraint is a Snare: it extracts (loss of chosen marriage) and suppresses (identity-fusion makes exit unthinkable even when the civil law permits it). The engine computes both readings from the structural data: d for the priesthood derives from beneficiary status + mobile exit (low d, beneficiary seat); d for the identity-locked individual derives from victim status + identity-lock exit (high d, target seat). This is exactly what tangled_rope is: one seat coordinated, one seat extracted from, by the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The priesthood (organized, mobile exit) benefits from the constraint without bearing its costs — they control ritual validity and derive authority from gatekeeping. They are beneficiaries. Parsi community leadership (organized, mobile exit) faces a mixed position: they benefit from the preserved community cohesion, but they also participate in the enforcement, giving them some responsibility for the extraction. I author them as beneficiary + secondary_role=agenda_setter to capture the dual position. Individuals choosing intermarriage (moderate power, identity_locked exit) are squarely the targets: they wish to marry outside the faith but face loss of family and community standing. Identity-lock is the suppression mechanism — they cannot exit the cultural identity even when the religious law constrains them; the constraint operates through internalized identity fusion. Non-Parsi spouses (moderate power, constrained exit) are collateral damage: they bear costs (exclusion from ritual, community ostracism) without having authored the constraint or benefited from it. Converts and would-be converts (powerless, trapped exit) face absolute gatekeeping: even those who wish to adopt Zoroastrianism are excluded by priesthood doctrine. Their trapped exit reflects the most severe suppression. Reform clergy (organized, mobile exit) are excluded stakeholders — they would argue for relaxed rules but are not in orthodox authority structures. Civil authorities (institutional, analytical exit) observe and can adjudicate, but currently the constraint persists because civil law in most jurisdictions treats religious family law as a matter of internal community governance. The directionality derivation from these positions should show high extraction for the identity-locked seats (d approaching 0.8), beneficiary positioning for the priesthood (d approaching 0.1–0.2), and excluded/observer status for reform clergy and civil authorities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preservation of a small diaspora against assimilation) was real and live for centuries. Its status is now contested: some attest it remains critical (orthodox priesthood); others attest it is substantially solved (reform clergy, secular scholars). The constraint persists regardless. The measurement series show extractiveness and suppression plateauing after t30, suggesting that the constraint is no longer extracting efficiently (resistance has grown, reform pressure is high, demographic decline continues despite enforcement). The theater_ratio plateau suggests the constraint is maintained more by institutional inertia than by active extraction benefit — the priesthood performs endogamy maintenance for community solidarity, not for rent-seeking, but the performance has become decoupled from the founding problem. This is not yet Piton (the priesthood still actively enforces gatekeeping, and the constraint is still legitimized through appeal to community survival). But the trajectory points toward Piton: if resistance continues to rise and the founding problem remains contested, the constraint may eventually be maintained only through theatrical performance, with no beneficiary collecting sufficient rent to sustain enforcement. The omega 'founding_problem_decay' captures this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_decay,
    'Is the constraint''s persistence justified by the founding problem (diaspora preservation in the face of assimilation pressure), or has the founding problem decayed to the point where the constraint is now purely boundary-maintaining theater?',
    'Longitudinal demographic analysis: does the Parsi population stabilize, decline, or grow under the endogamy regime versus reform-open scenarios in diaspora contexts? Comparative analysis with other small diaspora religions'' marriage rules and demographic outcomes.',
    'If the founding problem remains live, the constraint remains tangled_rope (justified extraction for a real coordination function). If the founding problem has decayed, the constraint should reclassify toward piton (maintained by inertia and institutional interest rather than genuine preservation benefit). A complete founding-problem dissolution + persistent enforcement = mandate-dead scenario.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_decay, empirical, 'Whether the founding problem (diaspora preservation against assimilation) remains live or has decayed while enforcement persists.').

omega_variable(
    identity_lock_mechanism,
    'Is the suppression of intermarriage primarily structural (priestly gatekeeping, community ostracism, ritual exclusion) or primarily internalized (Parsi identity is so fused with individual self-concept that exit is unthinkable even when civil law permits it)?',
    'Post-exit trajectory analysis: when Parsi individuals choose intermarriage and live outside the community for years, does the sense of cultural loss persist, or does it fade as they build new identity frames? Interviews with exogamists about whether they experienced suppression as external coercion or internal identity conflict.',
    'If suppression is primarily structural, the constraint''s effective extraction is what the direct gatekeeping measures (priestly denial of ritual, community councils'' exclusion). If suppression is primarily internalized (identity-fused), the constraint''s effective extraction is higher than the direct measures suggest, because the suppression persists after institutional mechanisms are removed. An internalized suppression scenario suggests the constraint has shaped identity itself, not merely institutional access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized suppression mechanism in endogamy enforcement.').

omega_variable(
    conversion_gatekeeping_vs_demographic,
    'Is the priesthood''s restriction on conversion a doctrinal position (grounded in Zoroastrian theology that racially/ethnically defines the faith community) or a strategic demographic choice (restricting conversion to preserve in-group marriage pools)?',
    'Textual analysis of Avesta and Pahlavi sources on proselytization versus contemporary priesthood statements on conversion policy. Historical comparison with pre-diaspora Zoroastrian conversion practices.',
    'If doctrinal, the conversion restriction is inseparable from Zoroastrian theology itself and is not a strategic extraction mechanism. If demographic-strategic, the restriction is a deliberate gatekeeping choice that amplifies the endogamy extraction by preventing even those who wish to enter the faith from doing so, and the constraint is more clearly extractive. A strategic-demographic verdict would strengthen the snare classification for this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conversion_gatekeeping_vs_demographic, conceptual, 'Whether conversion restriction is doctrinal or strategic in origin and intent.').

omega_variable(
    reform_clergy_countertrend,
    'Will progressive reinterpretation of Zoroastrian family law by reform clergy eventually displace orthodox gatekeeping, or will orthodox priesthood gatekeeping persist as the institutional standard despite reform movements?',
    '20-year longitudinal observation of Parsi community councils'' marriage law positions; tracking of reform clergy''s institutional influence and convert acceptance in different diaspora centers; demographic trends in communities that relax endogamy versus those that enforce it strictly.',
    'If reform prevails, the constraint''s extracted value decays rapidly, and the constraint may reclassify as piton (inertial, theatrical, maintained by a minority institutional position). If orthodox enforcement persists, the constraint remains tangled_rope with stable high extractiveness. A bifurcated outcome (reform in some diaspora centers, orthodox in others) would require decomposition into separate constraints per institutional context.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_clergy_countertrend, empirical, 'Whether reform reinterpretation of Zoroastrian law will displace orthodox gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(fami_tr_t0, observed).
narrative_ontology:measurement(fami_tr_t5, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(fami_tr_t5, observed).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(fami_tr_t10, observed).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(fami_tr_t15, observed).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(fami_tr_t20, observed).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(fami_tr_t25, observed).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(fami_tr_t30, observed).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(fami_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(fami_be_t0, observed).
narrative_ontology:measurement(fami_be_t5, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement_basis(fami_be_t5, observed).
narrative_ontology:measurement(fami_be_t10, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(fami_be_t10, observed).
narrative_ontology:measurement(fami_be_t15, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(fami_be_t15, observed).
narrative_ontology:measurement(fami_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(fami_be_t20, observed).
narrative_ontology:measurement(fami_be_t25, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(fami_be_t25, observed).
narrative_ontology:measurement(fami_be_t30, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(fami_be_t30, observed).
narrative_ontology:measurement(fami_be_t40, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(fami_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(fami_su_t0, observed).
narrative_ontology:measurement(fami_su_t5, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(fami_su_t5, observed).
narrative_ontology:measurement(fami_su_t10, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(fami_su_t10, observed).
narrative_ontology:measurement(fami_su_t15, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(fami_su_t15, observed).
narrative_ontology:measurement(fami_su_t20, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(fami_su_t20, observed).
narrative_ontology:measurement(fami_su_t25, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(fami_su_t25, observed).
narrative_ontology:measurement(fami_su_t30, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(fami_su_t30, observed).
narrative_ontology:measurement(fami_su_t40, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(fami_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the family_law_authority kernel. All five readings (Christian, Hindu, Muslim, Parsi Zoroastrian, Secular) are separate constraint stories with their own ε, beneficiary/victim structures, and readings_relations. This Parsi Zoroastrian reading emphasizes small-community boundary preservation through endogamy and priesthood authority. It coexists with the other readings (different parties hold each simultaneously) and influences them (the existence of a strong religious-law reading creates pressure on secular readings to accommodate religious autonomy). Each reading has its own cs_structure with reading_relations to the siblings and foundational axioms unique to that reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
