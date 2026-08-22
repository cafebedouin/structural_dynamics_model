% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Code Coordination Under Legal Suppression (Practice Decline Reading)
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   This reading of the honor_satisfaction_substrate kernel frames the
 *   decline of dueling as a result of exogenous legal and institutional
 *   enforcement rather than as an endogenous transformation of what 'honor'
 *   itself means. Under this reading, the honor code persists as a live
 *   normative system — participants continue to believe in the rightness of
 *   the honor framework and the necessity of costly-signal responses to
 *   affronts — but the behavioral instantiation (dueling) becomes impractical
 *   due to legal prohibition, institutional barriers, and opportunity costs
 *   imposed by states and formal organizations. Dueling remains thinkable but
 *   not doable. Honor codes survive in attenuated forms (military honor
 *   codes, Southern cultures of honor) that preserve the reputation-signaling
 *   function without the violence. The constraint is classified as rope
 *   (coordination under pressure) rather than mountain erosion (the code
 *   losing all meaning) or piton (the code becoming purely performative). The
 *   key structural claim: the normative substrate remains intact; only the
 *   practice is suppressed.
 *
 * KEY AGENTS:
 *   - honor_community_participants: elite networked actors (military, gentry, professionals) who are beneficiaries of honor-code coordination but also the targets of legal suppression when they engage in dueling
 *   - coerced_duelers: specific individuals caught between honor's demand and law's prohibition, bearing the direct cost of the constraint's operation
 *   - legal_authorities: institutional agenda-setters imposing the suppression through criminalization and prosecution
 *   - military_hierarchy: dual-positioned institutional actor maintaining shadow honor codes while formally suppressing dueling
 *   - status_aspirants: excluded from the coordination mechanism by birth/rank but affected by its existence as a barrier
 *   - women: structurally central to honor (as causes and judges of disputes) but excluded from participation
 *   - observer_legal_historians: analytical seat measuring the practice/normativity gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.62).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.78).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Code Coordination Under Legal Suppression (Practice Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, '62277b26-5fa9-41c0-8178-467d2576efe7').
narrative_ontology:cs_kernel_codification('62277b26-5fa9-41c0-8178-467d2576efe7', distributed).
narrative_ontology:cs_authority_grounding('62277b26-5fa9-41c0-8178-467d2576efe7', practice).
narrative_ontology:cs_interpretation_layer_present('62277b26-5fa9-41c0-8178-467d2576efe7').
narrative_ontology:cs_reading_relation('62277b26-5fa9-41c0-8178-467d2576efe7', honor_satisfaction_substrate__cultural_contraction_reading, influences).
narrative_ontology:cs_reading_relation('62277b26-5fa9-41c0-8178-467d2576efe7', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('62277b26-5fa9-41c0-8178-467d2576efe7', foundational, honor_code_normativity_survives_practice_suppression).
narrative_ontology:cs_axiom_status(honor_code_normativity_survives_practice_suppression, holdable).
narrative_ontology:cs_axiom_grounding('62277b26-5fa9-41c0-8178-467d2576efe7', honor_code_normativity_survives_practice_suppression, conventional).
narrative_ontology:cs_axiom('62277b26-5fa9-41c0-8178-467d2576efe7', foundational, exogenous_enforcement_causes_behavioral_decline_not_normative_collapse).
narrative_ontology:cs_axiom_status(exogenous_enforcement_causes_behavioral_decline_not_normative_collapse, holdable).
narrative_ontology:cs_axiom_grounding('62277b26-5fa9-41c0-8178-467d2576efe7', exogenous_enforcement_causes_behavioral_decline_not_normative_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('62277b26-5fa9-41c0-8178-467d2576efe7', intact_honor_code_under_legal_attack).
narrative_ontology:cs_drift_state('62277b26-5fa9-41c0-8178-467d2576efe7', nineteenth_century_suppression_epoch, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62277b26-5fa9-41c0-8178-467d2576efe7', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_community_participants).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, coerced_duelers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_hierarchy).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_community_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of gentry, military, and elite occupational networks who gain reputation, social standing, and coalition strength from participation in the honor code. They benefit from the coordination function (clear status signals, reliable reputation tracking, predictable consequences for slights). They also pay through the necessity of accepting dueling challenges under honor rules — a constant background threat that structures daily social performance. Exit requires severing identity as a gentleman or officer, which for most is unthinkable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_community_participants, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, honor_community_participants, payer).

% Specific individuals who face legal liability, personal injury, or social death if they accept a duel, but social erasure if they refuse. The legal prohibition creates a trap: honor demands response to slights, but law prohibits the response mechanism. They bear the cost of the constraint's enforcement (legal punishment for dueling) while remaining under honor's demand structure.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, coerced_duelers, payer,
    moderate, biographical, constrained, regional).

% States that criminalize dueling and prosecute those who participate. They enforce the constraint through law, courts, and punishment. From their position the constraint is not primarily about honor coordination but about monopolizing legitimate violence and preventing elite-driven private justice systems that compete with state authority.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Military institutions that derive officer cohesion and battlefield trust from honor codes but formally participate in their suppression through official prohibitions on dueling. They face competing demands: honor codes strengthen unit cohesion and discipline, but dueling undermines chain of command and removes officers at unpredictable times. They maintain shadow honor codes while participating in their formal suppression.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_hierarchy, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_hierarchy, beneficiary).

% Non-elite individuals (merchants, professionals, artisans) excluded from the formal honor community by birth but who observe honor dynamics and would participate if barriers dropped. They are kept outside by the constraint's gatekeeping function and would have standing to challenge if dueling were available as a reputation mechanism.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, status_aspirants, excluded,
    powerless, biographical, constrained, regional).

% Formally outside the dueling code (as non-participants) but structurally central to honor disputes (as repositories of family honor, causes of disputes, judges of male reputation). They cannot resolve honor through dueling themselves but are both causes and audiences of male duels. Their exclusion from the participation mechanism while remaining central to honor stakes structures the entire system.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, women, excluded,
    powerless, biographical, trapped, regional).

% Scholarly and institutional analysts examining the decline of dueling and the persistence of honor codes. They see the constraint's operation from outside the stakes and can measure the gap between normative persistence and behavioral decline.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, observer_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__practice_decline_reading, legal_authorities).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__practice_decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of reputation verification in elite networks lacking centralized institutional scorekeeping. A credible response to slights (violent challenge) signals genuine offense-taking and status consciousness; declining signals cowardice or irrelevance. The duel is the costly-signal mechanism that makes honor claims credible in zero-credentialing contexts.
% TRANSFER_FUNCTION: Transfers risk and injury from disputants to the constraint's enforcement machinery (legal penalties, social ostracism for refusal). The arrangement also transfers power from individual dispute resolution to state-monopolized justice, reducing elite private authority over reputation maintenance.
% ABSENT_VOICES: Status aspirants (merchants, professionals without elite birth) who would use dueling as a cross-status reputation mechanism if barriers dropped; women who are central to honor stakes but excluded from the participation mechanism; lower-status soldiers who are affected by officer-code dueling but have no say in its maintenance; non-elite publics who bear indirect costs of elite dueling (deaths of talented individuals, social instability).
% DISAPPEARANCE_RATIONALE: If the honor coordination constraint vanished (i.e., if honor code lost normative force entirely), elite reputation markets would reorganize: formal credentials, institutional affiliation, and written record would accelerate as substitutes for costly-signal dueling. Military units would rely more on command authority and less on officer-cohort honor ties. Social slights would be handled through gossip, written response, or legal complaint rather than challenge. The transition would be jarring but not chaotic — institutional and market mechanisms exist to absorb reputation functions.
% FOUNDING_PROBLEM: Elite social networks required a credible way to signal genuine status consciousness and serious offense-taking in response to affronts. Written law and institutions were underdeveloped; reputation was the primary form of social capital. A formalized response (the duel) created a costly signal that separated credible offense-takers from poseurs.
% FOUNDING_PROBLEM_CORROBORATION: Dueling historians (Kiernan, McAleer) and legal scholars (Esposito) attest the founding problem (coordination of reputation in status-conscious networks without institutional alternatives) was real and central. Legal authorities and state institutional historians attest the founding problem is obsolete given modern credential systems and institutional rank structures. Status aspirants would attest that the founding problem solved an elite-specific coordination crisis, not a general problem of reputation. Women would note that the founding problem treated male reputation as a public good worth violent resolution while female reputation was managed through other mechanisms entirely. Composite_overdetermined_reading scholars would note that the founding problem presupposes an older cultural framework (shame-honor cultures) that was already in transition during the dueling period, so the 'problem' and its solution were co-transforming.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__practice_decline_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Under this reading, extractiveness rises from 0.38 (1600, when legal suppression was minimal and honor coordination was largely uncontested) to 0.62 (1900, when legal suppression is routine and the honor code operates under constant constraint). The suppression_requirement metric shows corresponding rise from 0.25 to 0.78, reflecting increasing legal and institutional enforcement machinery. Theater_ratio rises from 0.15 to 0.41, indicating that honor-code performance becomes increasingly theatrical as the practice is suppressed and participants maintain the normativity while abandoning or hiding the behavior. Accessibility_collapse (0.68) reflects the fact that once one is embedded in the honor community, the alternatives (refusing challenges, abandoning status) collapse nearly completely due to identity-lock — you cannot exit the honor frame without ceasing to be a gentleman or officer. Resistance (0.72) is substantial because honor-community participants continue to believe in the rightness of the code and many engage in clandestine dueling despite legal prohibition. The claimed_type (rope) reflects the core claim: this is genuine coordination (reputation signaling under information scarcity) with active enforcement pressure (legal suppression) — rope under legal constraint, not mountain because the normativity is contested and contingent, not because the code is naturally inevitable.
 *
 * PERSPECTIVAL GAP:
 *   The honor-community participant and legal authority should compute drastically different classifications. From the participant's seat: this is rope-like coordination (I need this system; legal suppression is an external nuisance interfering with legitimate reputation signaling). From the legal authority's seat: this is suppressed extraction (I am working to eliminate a system that allows non-state actors to settle disputes privately and outside my authority). The engine computes both seats' perceptions from the structural data; the authored metrics do not predetermine which seat's framing prevails. This reading strategically selects the participant perspective (the honor community 'sees' this as rope under suppression) rather than the legal-authority perspective (which sees it as illegitimate extraction to be eliminated).
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-community participants derive genuine value from the coordination function while remaining trapped in its enforcement structure. Their identity_locked exit option (they cannot leave without ceasing to be gentlemen or officers) creates strong directionality toward target-status despite the coordination benefit. Legal authorities are firmly on the beneficiary side of the suppression mechanism — they gain monopoly enforcement authority. The power differential matters: institutional legal authorities can sustain suppression indefinitely, while moderate-power honor participants are perpetually constrained. Coerced_duelers have constrained (not identity_locked) exit — they could theoretically leave the honor community if legal suppression were lifted, but under suppression they are trapped between two incompatible demands.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading addresses a key mandatrophy question: does the honor code represent a founding mandate that has outlived its function (making it a candidate for piton classification), or does it represent a live coordination mechanism under external suppression (making it rope)? Under this reading, the mandate (provide reputation coordination in the absence of centralized institutional scorekeeping) remains functionally live — participants continue to believe the system serves a real function — even though the behavioral manifestation (dueling) has been suppressed. The constraint avoids mandatrophy classification because the normative substrate persists. A rival reading (cultural_contraction_reading) would argue the mandate itself has become incoherent due to cultural transformation in what 'honor' means; that reading would support mandatrophy (the code is dead normativity). This reading prevents mandatrophy classification by claiming the normativity is intact and only the practice is suppressed. The measurement series (theater_ratio rising to 0.41 but not approaching 0.8+) supports the reading's claim: some performance (theater) is present, but it is not the dominant mode — honor codes still function as coordination, not as pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_delegitimation,
    'Did the honor code lose legitimacy primarily through exogenous legal/institutional suppression (this reading''s claim), or through endogenous cultural transformation of what ''honor'' itself means (cultural_contraction_reading''s claim)?',
    'Textual and interview analysis: examine whether honor-code participants in the 1800s describe their constraint as externally coerced but normatively intact (supporting this reading) or as internally incoherent and meaningless (supporting the alternative). Analyze rhetoric of officers, gentlemen, and honor-community members during the suppression period.',
    'If exogenous, the honor code remains a live coordination mechanism suppressed by law — a rope under enforcement pressure. If endogenous, the constraint transitions to a mountain (honor code meaninglessness becomes structural fact) or a piton (performative maintenance of a dead function). Classification hinges on this factual distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_delegitimation, empirical, 'Whether dueling decline was caused by external enforcement or internal delegitimation of honor itself.').

omega_variable(
    causal_pathway_independence,
    'Were the exogenous suppression (legal prohibition) and endogenous delegitimation (cultural transformation) independent causal pathways, or did they interact such that understanding one requires understanding the other?',
    'Comparative historical analysis: examine jurisdictions with early legal prohibition but persistent honor culture (e.g., some US Southern contexts) versus jurisdictions with weak legal enforcement but early cultural shift away from honor (e.g., some Northern/industrial contexts). If patterns diverge, pathways are independent; if they co-vary, interaction is present.',
    'If pathways are independent, this reading (exogenous suppression) and the cultural_contraction_reading (endogenous delegitimation) are genuinely separable; if entangled, the composite_overdetermined_reading is more accurate and this reading is a partial perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_pathway_independence, empirical, 'Whether suppression and cultural transformation were causally independent or entangled.').

omega_variable(
    practice_normativity_gap_mechanism,
    'When honor codes persisted normatively while dueling practice declined, was the gap maintained by (a) identity-locked participants who internalized the suppression as illegitimate authority, or (b) theatrical performance by participants who no longer believed in honor but sustained the code performatively?',
    'Textual analysis of correspondence, diaries, and period commentary: do surviving texts describe internalized resistance and continued belief in honor''s rightness despite legal prohibition, or describe honor-code performance as increasingly hollow and ironic? Post-suppression oral history where available.',
    'If (a), the suppression acts on an intact but constrained normative system — rope under enforcement. If (b), the suppression accelerates the transition toward piton (theater-dominated operation). The mechanisms imply different long-term trajectories for the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_normativity_gap_mechanism, empirical, 'Whether the gap between honor-code normativity and dueling practice represented internalized constraint or performative maintenance.').

omega_variable(
    alternative_constraint_family_reading,
    'This story instantiates the practice_decline_reading of the honor_satisfaction_substrate kernel. A sibling reading (composite_overdetermined_reading) would claim that exogenous and endogenous factors were causally entangled and non-decomposable. Does the exogenous suppression you are authoring actually presuppose an endogenous shift already underway, such that the two readings are not genuinely alternative but rather different phrasings of a single overdetermined phenomenon?',
    'Temporal analysis: establish whether legal prohibition preceded cultural shift in belief (exogenous primary), cultural shift preceded legal codification (endogenous primary), or both occurred simultaneously without clear precedence (overdetermined). Examine earliest prohibition statutes relative to earliest expressions of cultural doubt about honor.',
    'If exogenous clearly precedes endogenous, this reading is defensible as a genuine alternative to the composite reading. If they are truly simultaneous or tangled, the composite_overdetermined_reading better captures the actual causal structure, and this reading becomes a simplified perspective on a more complex phenomenon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_constraint_family_reading, conceptual, 'Whether the practice_decline_reading can coherently claim exogenous suppression as the primary cause, or whether it is a partial perspective on an overdetermined phenomenon.').

omega_variable(
    kernel_reading_identity_ambiguity,
    'Is this reading of the honor_satisfaction_substrate kernel genuinely distinct from the sibling composite_overdetermined_reading, or is it a reframing of the same phenomenon that presupposes part of the composite story (i.e., does ''exogenous suppression'' require a prior shift in cultural receptivity for the suppression to ''stick'')?',
    'Construct-validity analysis: if legal suppression of dueling succeeded where earlier suppression attempts failed (e.g., church prohibition in earlier centuries), what changed? If cultural receptivity to anti-dueling arguments changed before (or caused) legal codification, then exogenous suppression presupposes endogenous cultural shift, and this reading becomes a post-hoc perspective on a prior transformation that this reading attributes to external causes.',
    'If the reading presupposes composite causation, the distinction between practice_decline_reading and composite_overdetermined_reading collapses, and this reading is a valid but incomplete perspective on the constraint. If exogenous suppression can be disentangled from prior endogenous shifts, this reading is independently defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_ambiguity, conceptual, 'Whether the practice_decline_reading''s exogenous suppression claim can be isolated from composite causation or presupposes it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1600, 0.15).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1700, 0.22).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1750, 0.28).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1800, 0.35).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1850, 0.4).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1900, 0.41).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1600, 0.38).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1700, 0.48).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1750, 0.52).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1850, 0.62).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1900, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1600, 0.25).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1700, 0.42).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1750, 0.55).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1800, 0.68).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1850, 0.75).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1900, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__practice_decline_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (practice_decline_reading) of a contested kernel (honor_satisfaction_substrate) with two sibling readings: cultural_contraction_reading (endogenous transformation of honor concept itself) and composite_overdetermined_reading (exogenous and endogenous factors causally entangled). The three stories represent genuinely alternative framings of the same historical phenomenon with different causal structures and different terminal classifications. Each is authored as an ε-invariant constraint with its own beneficiary/victim structure. Network links document the constraint family; omegas document where the readings diverge and what evidence would resolve the disagreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
