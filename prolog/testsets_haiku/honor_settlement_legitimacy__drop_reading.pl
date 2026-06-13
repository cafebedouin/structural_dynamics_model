% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Honor-Based Dispute Settlement (Drop Reading: Fringe Persistence)
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   This constraint instantiates the DROP READING of the
 *   honor-settlement-legitimacy kernel: a reading that holds honor culture
 *   remains a live evaluative framework in specific geographic and social
 *   niches (certain military castes, regional aristocracies, diaspora
 *   communities), where dueling persists as a fringe practice despite legal
 *   prohibition spanning two centuries. The reading is distinct from the
 *   CONTRACTION reading (which holds honor became cognitively unthinkable)
 *   and the COMPOSITE reading (which holds dueling's decline was
 *   overdetermined by multiple mechanisms). In the drop reading, honor is
 *   suppressed by law and institutional pressure, but not eliminated from the
 *   normative repertoire of residual communities. The constraint's operation
 *   at year 2000 shows high suppression (0.72—active enforcement is required
 *   to keep the practice clandestine), rising theater ratio (0.58—the energy
 *   expenditure is increasingly about performing the prohibition rather than
 *   defending the mechanism's legitimacy), and declining but persistent
 *   extractiveness (0.31—the constraint still extracts from peripheral
 *   participants and demands state enforcement attention, but offers
 *   diminishing coordination benefit even within its residual communities).
 *   The extractiveness decline coupled with rising suppression and theater is
 *   the piton signature: a constraint maintained by institutional inertia and
 *   identity attachment rather than by ongoing coordination value or active
 *   rent-seeking.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: residual communities (military castes, regional aristocracies, diaspora groups) maintaining honor-settlement as identity marker and disputed dispute-mechanism. Power: organized. Time horizon: generational. Exit: identity_locked.
 *   - peripheral_dueling_participants: individuals drawn into disputes within honor frames, bearing legal and social risk. Power: moderate. Time horizon: biographical. Exit: constrained.
 *   - legal_jurisdictions: state legal systems enforcing prohibition, bearing cost of active suppression. Power: institutional. Time horizon: generational. Exit: analytical.
 *   - non_honor_culture_majority: broader population benefiting from legal-dispute normalization. Power: institutional. Time horizon: generational. Exit: analytical.
 *   - honor_culture_external_critics: reformers/scholars outside residual communities, excluded from inside-reframing influence. Power: moderate. Time horizon: biographical. Exit: mobile.
 *   - ethnographic_observer: analytical seat documenting persistence pattern. Power: analytical. Time horizon: civilizational. Exit: analytical.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.31).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.72).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor-Based Dispute Settlement (Drop Reading: Fringe Persistence)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, '99c9d3f8-9362-4421-9c17-737d80cec032').
narrative_ontology:cs_kernel_codification('99c9d3f8-9362-4421-9c17-737d80cec032', distributed).
narrative_ontology:cs_authority_grounding('99c9d3f8-9362-4421-9c17-737d80cec032', extraction).
narrative_ontology:cs_reading_relation('99c9d3f8-9362-4421-9c17-737d80cec032', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('99c9d3f8-9362-4421-9c17-737d80cec032', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('99c9d3f8-9362-4421-9c17-737d80cec032', foundational, honor_legitimacy_persists_in_residue).
narrative_ontology:cs_axiom_status(honor_legitimacy_persists_in_residue, holdable).
narrative_ontology:cs_axiom_grounding('99c9d3f8-9362-4421-9c17-737d80cec032', honor_legitimacy_persists_in_residue, conventional).
narrative_ontology:cs_axiom('99c9d3f8-9362-4421-9c17-737d80cec032', secondary, legal_suppression_incompleteness_structural).
narrative_ontology:cs_axiom_status(legal_suppression_incompleteness_structural, holdable).
narrative_ontology:cs_axiom_grounding('99c9d3f8-9362-4421-9c17-737d80cec032', legal_suppression_incompleteness_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('99c9d3f8-9362-4421-9c17-737d80cec032', honor_culture_legitimacy_persists_in_pockets).
narrative_ontology:cs_drift_state('99c9d3f8-9362-4421-9c17-737d80cec032', contemporary_legal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('99c9d3f8-9362-4421-9c17-737d80cec032', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, peripheral_dueling_participants).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, legal_jurisdictions_enforcing_bans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, non_honor_culture_majority).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, honor_code_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, masculine_reputation_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of residual honor-based communities (certain military castes, regional aristocracies, diaspora groups maintaining pre-legal dispute norms) who view dueling as a legitimate mechanism for settling disputes about honor and reputation. They maintain the practice in clandestine form, invoke it rhetorically within their communities, and transmit it to younger generations as part of identity maintenance. They are simultaneously constrained by legal prohibition and motivated to preserve the mechanism as a marker of cultural continuity.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_culture_practitioners, agenda_setter,
    organized, generational, identity_locked, regional).

% Individuals drawn into honor-settlement disputes (often younger members of honor cultures, sometimes outsiders marrying into these communities) who face social and sometimes legal consequences of participating in dueling. They bear the direct risk: physical injury, criminal prosecution, social ostracization if they refuse (loss of reputation in the honor frame) or accept (legal jeopardy). Their exit is constrained because reputation damage within their community circle is irreversible.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, peripheral_dueling_participants, payer,
    moderate, biographical, constrained, regional).

% State legal systems that criminalized dueling centuries ago and continue to enforce prohibitions against it. They must actively suppress the practice through prosecution, investigation, and public messaging (the theater component). The suppression requires continuous enforcement machinery because the practice does not spontaneously disappear from the normative repertoire of certain subgroups; the state bears the cost of maintaining prohibition enforcement against residual demand.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, legal_jurisdictions_enforcing_bans, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, legal_jurisdictions_enforcing_bans, payer).

% The broader population living under legal systems that have delegitimized private-violence dispute settlement. They benefit from the normative triumph of legal over honor-based settlement, though the benefit is diffuse and abstract (living in a jurisdiction where reputation disputes are handled by courts, not fatal encounters). They do not actively maintain the legal prohibition; they take it as background normality.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, non_honor_culture_majority, beneficiary,
    institutional, generational, analytical, national).

% Reformers, feminist advocates, and legal scholars who argue that honor-culture frameworks perpetuate dangerous patriarchal norms and violence. They would advocate for deeper cultural intervention (education, normative reframing, identity-alternative pathways) but are largely outside the communities where honor dueling survives, giving their intervention limited purchase within those insider communities. Their voice would reshape the legitimacy narrative if heard inside, but structural distance limits influence.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_culture_external_critics, excluded,
    moderate, biographical, mobile, national).

% Anthropologists, historians, and legal scholars who document the persistence of honor-settlement norms in specific geographic and cultural niches. They occupy the analytical seat—neither benefiting from nor bearing the direct costs of the constraint's operation, but capable of seeing the full structural pattern across communities and time.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, ethnographic_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the drop reading, dueling functioned historically as a dispute-settlement mechanism for honor-related conflicts (reputation, insult, precedence claims) that occurred outside or parallel to formal legal channels. It was the coordination solution for conflicts whose resolution depended on observable demonstration of courage/willingness-to-risk rather than external judgment. The reading frames this function as PERSISTING in residual form within specific communities, not as obsolete—the coordination problem (how to settle reputation disputes in honor-culture frames) remains live for those communities even as the broader society has delegitimized it.
% TRANSFER_FUNCTION: The constraint transfers reputational capital and bodily risk from honor-culture practitioners (who gain status, maintain cultural coherence, and transmit identity) to peripheral dueling participants and to legal jurisdictions (who bear the costs of enforcement, investigation, prosecution, and community surveillance). The transfer is partly extracted rent (practitioners benefit from framing honor as unsettled by anything other than dueling) and partly coordination cost (the mechanism itself demands participant sacrifice to function).
% ABSENT_VOICES: Honor-culture external critics (reformers, feminist scholars, alternative-identity advocates) are structurally excluded from the communities where honor settlement persists, limiting their ability to reshape the legitimacy narrative from inside. Victims of dueling who do not belong to honor cultures (bystanders, family members opposed to the practice, participants coerced into participation) have no formal voice in how the constraint is maintained or transmitted.
% DISAPPEARANCE_RATIONALE: The drop reading holds that if honor-settlement legitimacy disappeared (the normative claim that dueling is a valid way to resolve reputation conflicts), the communities that practice it would lose a core identity marker and would need alternative mechanisms for status negotiation and reputation repair. The state would face less enforcement burden. But whether the disappearance would be permanent or incomplete is contested—the reading itself asserts that honor culture remains cognitively live in some circles, suggesting that mere legal suppression without deep cultural reframing leaves the normative demand unresolved, and the practice persists through clandestine channels.
% FOUNDING_PROBLEM: In the drop reading, the founding problem is: how do honor-based communities settle disputes about reputation, precedence, and insult when formal legal systems (1) were not available or (2) did not recognize honor-based claims as legitimate? Dueling emerged as the solution because it allowed observable demonstration of commitment (willingness to fight) as a proxy for truth/righteousness in honor frames.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists attest that the practical founding problem—absence of formal legal mechanisms for reputation disputes—is dead in jurisdictions with functional court systems. However, cultural historians and anthropologists who study residual honor communities attest that the NORMATIVE problem persists: communities maintain the conviction that certain kinds of honor can only be settled through demonstration of courage, not through legal judgment. The founding problem's practical half is solved; its normative half persists in pockets. This split attestation is the reading's core tension.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).
:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the drop reading's core claim: honor is NOT dead (cognitive frame remains live in pockets) but IS suppressed by law and social pressure. Extractiveness starts at 0.58 in 1800 (strong coordination value + powerful rent from reputation control by honor practitioners) and declines to 0.31 by 2000 (the coordination function is marginalized; modern participants mostly pay to maintain identity and status within their community, not because the mechanism solves an active coordination problem). Suppression RISES from 0.42 to 0.72 over the interval, tracking the tightening of legal and institutional enforcement. Theater RISES from 0.15 to 0.58, indicating that an increasing share of enforcement activity is performative—messaging the prohibition, policing visibility, managing cultural narrative—rather than defending the mechanism's legitimacy. This signature (declining extractiveness + rising suppression + rising theater) is diagnostic of a piton: an institution that persists despite low utility because it is embedded in identity and because dismantling it would require addressing the cultural attachment, which the state finds easier to suppress theatrically than to resolve.
 *
 * PERSPECTIVAL GAP:
 *   The gap between agenda-setter seats (honor practitioners + legal jurisdictions) and payer seats (peripheral participants) should compute starkly: from the honor-practitioner perspective, the constraint is identity-coordination with optional participation (live cultural option, choose your community). From the peripheral-participant perspective, it is coercive extraction (you are born into a frame that makes participation obligatory; exit is identity death or legal jeopardy). From the legal-jurisdiction perspective, it is institutional maintenance (manage a residual threat to legal order, deploy proportionate enforcement). The engine computes these seat-specific types from the structural data—the power atoms differ, the exit options differ, the relationship to the beneficiaries differs. The authored claim (piton) and metrics (rising theater, declining utility) are independent of these computed per-seat types; the divergence between claim and metrics is exactly what the measurement is designed to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor culture practitioners sit near the beneficiary end (d near 0.2–0.3): they maintain the frame, transmit it, and derive identity/status from it, even though legal suppression constrains their operationalization. Peripheral participants sit at the target end (d near 0.8–0.9): they face legal jeopardy, social pressure, and identity-fusion coercion (if they refuse to participate, they are seen as dishonorable within their communities). Legal jurisdictions are divided: as enforcement agents, they are positioned as targets of coordination costs (d near 0.7); as institutional beneficiaries of legal-order legitimacy, they sit as asymmetric beneficiaries (d near 0.3). The non-honor-culture majority sit near full beneficiary (d near 0.1): they live in a legal regime where reputation disputes are handled through courts, they face no participatory costs, and the suppression of honor-settlement benefits them diffusely. The external critics are analytical (d near 0.5): they would benefit from deeper cultural change but are not parties to the enforcement mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to settle honor-based reputation disputes without legal infrastructure) is dead—modern legal systems handle reputation claims through defamation law and civil courts. But the normative claim (that honor can ONLY be settled through direct demonstration of courage) persists in residual communities. The drop reading locates mandatrophy at the boundary: the coordination problem the constraint was built to solve no longer exists in the broader population, but persists in identity form within residual communities. The state's suppression machinery exists not to solve a coordination problem but to manage the persistence of an obsolete value frame. The rising theater ratio (0.58 at endpoint) is the clinical sign: the state is increasingly spending enforcement energy on managing narrative and visibility rather than on addressing any functional deficit. This is pure piton maintenance—institutional inertia and identity attachment sustain it, not coordination value or active rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_culture_cognitive_autonomy,
    'Is honor-culture framing truly persistent as a live evaluative framework within residual communities, or has it become performative theater—a cultural identity assertion rather than a genuine belief in the legitimacy of honor settlement?',
    'Ethnographic deep-dive into contemporary honor-culture practitioners: interview intergenerational transmission, examine actual dispute-handling choices when dueling is not genuinely an option (do practitioners use honor rhetoric but defer to legal resolution, or do they maintain clandestine dueling as preferred mechanism). Assess whether participants describe honor-settlement as obligatory truth or as valued tradition.',
    'If honor framing is theater (identity marker, not operative belief), the constraint''s type shifts toward pure piton: maintained by cultural inertia, not active legitimacy claim. If live cognitive framework, the extraction is hybrid—some share is genuine coordination cost for those who believe in it, some is coercive imposition on those who do not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_culture_cognitive_autonomy, empirical, 'Whether residual honor-culture practitioners maintain operative belief in honor legitimacy or performative identity claim.').

omega_variable(
    reading_contraction_vs_drop_boundary,
    'Is the drop reading (honor persists as fringe live option) empirically distinct from the contraction reading (honor became cognitively unthinkable), or do they describe the same phenomenon from different temporal perspectives—i.e., is the drop reading just the contraction reading before its final completion?',
    'Longitudinal cognitive-frame analysis: measure the proportion of the population in jurisdiction X that holds honor-settlement-as-legitimate as an evaluative frame at T1 (1850), T2 (1920), T3 (1980), T4 (2020). If the proportion asymptotes to a small positive residue (fringe but stable), drop reading holds. If it continues monotonic decline toward zero, contraction reading holds (drop is a way-station on contraction''s path). Frame operationalization: ''would you recommend honor dueling as a valid way to settle a reputation dispute'' measured in representative survey of diverse community samples.',
    'If residue is stable, drop is a distinct reading and the constraint genuinely operates as piton (maintained by inertia despite low practical utility). If monotonic decline, drop is a narratively distinct but structurally transient reading; the underlying constraint is contracting and drop describes its penultimate phase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contraction_vs_drop_boundary, empirical, 'Whether honor-culture framing stabilizes at a residual level or continues monotonic decline.').

omega_variable(
    identity_lock_mechanism_internalization,
    'When peripheral dueling participants face the choice to participate or exit, how much of the suppression operates through structural barriers (legal prosecution, community isolation if they refuse) versus internalized shame/honor identity fusion (they believe they SHOULD participate because it is who they are)?',
    'Post-exit trajectory analysis: study participants who left honor-culture communities (emigration, deconversion, generational shift). Do they report that the constraint continues to operate internally (shame, sense of cowardice, identity rupture) after they leave, or does suppression cease once structural enforcement stops? Internalization signals (continued felt obligation despite legal/community safety from participation; emotional distress at identity reframing; continued narrative invocation of honor even in contexts where audience does not share the frame).',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (0.72) suggests; participants carry the suppression with them and may transmit it intergenerationally even outside enforcement jurisdictions. If structural, suppression is context-dependent and resets when enforcement context changes. The distinction affects whether the constraint is primarily about transmitted identity (harder to dislodge) or about coercive maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Structural versus internalized mechanisms in honor-culture suppression.').

omega_variable(
    reading_kernel_contest_simultaneity,
    'Are the three readings of the honor-settlement-legitimacy kernel (composite, contraction, drop) describing the SAME constraint evaluated at different time points, or are they describing genuinely DIFFERENT constraints coexisting in the same domain?',
    'Specification audit: For each reading, isolate the ε value, the beneficiary/victim structure, and the enforcement mechanism. If ε, beneficiary set, and enforcement all shift across readings, they may be describing different constraints. If the same structural relationships hold but the narrative frame and temporal trajectory differ, they are readings of one kernel. Kernel-test: if adopting one reading''s reference frame precludes adopting another''s, they foreclose; if not, they coexist or influence.',
    'If truly different constraints, each reading should be a separate story. If readings of one kernel, they share a constraint_id family and differ only in cs_structure reference_frame and drift_state. Correct classification affects corpus genealogy indexing—competing hypotheses about one phenomenon (kernel readings) versus distinct phenomena with causal relationships (constraint family).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest_simultaneity, conceptual, 'Whether the three declared readings are readings of one kernel or distinct constraints in the honor-settlement domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1800, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__drop_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__drop_reading, theater_ratio, 1850, 0.28).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__drop_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__drop_reading, theater_ratio, 1950, 0.52).
narrative_ontology:measurement_basis(hono_tr_t1950, observed).
narrative_ontology:measurement(hono_tr_t2000, honor_settlement_legitimacy__drop_reading, theater_ratio, 2000, 0.58).
narrative_ontology:measurement_basis(hono_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1850, 0.48).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(hono_be_t1900, observed).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1950, 0.32).
narrative_ontology:measurement_basis(hono_be_t1950, observed).
narrative_ontology:measurement(hono_be_t2000, honor_settlement_legitimacy__drop_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement_basis(hono_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1850, 0.54).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1900, 0.66).
narrative_ontology:measurement_basis(hono_su_t1900, observed).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement_basis(hono_su_t1950, observed).
narrative_ontology:measurement(hono_su_t2000, honor_settlement_legitimacy__drop_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement_basis(hono_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__drop_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The honor-settlement-legitimacy kernel admits three structural readings: DROP (this story—honor persists as live option in residual communities, suppressed but not eliminated), CONTRACTION (honor became cognitively unthinkable through cultural framework transformation), and COMPOSITE (overdetermined decline via multiple mechanisms). The three readings differ in reference_frame and drift_state within cs_structure; they share the same core constraint_id family. Each reading has distinct ε and beneficiary/victim configurations reflecting its empirical claim about honor's persistence. All three readings affect one another through the legitimacy dynamics they instantiate—the contraction reading's claim (honor became unthinkable) influences the drop reading's viability (if contraction is true, drop would describe a transient phase); the composite reading's overdetermination hypothesis influences both by asserting multiple causal paths to the observed outcome. Drop is the persistence-resistant reading and occupies the empirically most contentious position: it asserts stable residue where contraction asserts monotonic elimination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__drop_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
