% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta as Feudal Artifact (Obsolescence Reading)
 *   domain: constitutional_history/legal_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the feudal-obsolescence reading of the
 *   contested Magna Carta kernel. Under this reading, Magna Carta is treated
 *   as a baronial compact addressing 13th-century feudal grievances with no
 *   binding authority over modern sovereignty structures. The reading holds
 *   that modernization, statutory law, written constitutions, and the
 *   transition from feudal to nation-state legal systems have rendered the
 *   charter operationally inert. This reading is prevalent in
 *   executive-branch and legal-positivist circles and dominates modern
 *   constitutional doctrine in many common-law jurisdictions. The constraint
 *   measured here is the effective authority structure this reading produces:
 *   a state of affairs where Magna Carta is invoked theatrically by
 *   constitutionalists but suppressed by courts and executive officials who
 *   treat it as historically superseded. The measurement series captures the
 *   accumulation of this extractive dynamic over 810 years, showing rising
 *   theater, rising suppression, and rising effective extractiveness as the
 *   obsolescence frame consolidated institutional authority.
 *
 * KEY AGENTS:
 *   - Executive power seat: maintains and administers the obsolescence narrative; claims Magna Carta is feudal artifact with no modern force
 *   - Popular constitutionalists and juridical restraint advocates: bear costs of reduced precedent access; face suppression through dismissal as anachronistic
 *   - Common-law litigants: structurally prevented from invoking charter in modern litigation; forced to rely on statutory protections alone
 *   - Academic constitutionalists: excluded from policy discourse; research agenda marginalized as historical nostalgia
 *   - Comparative constitutional systems: external observers showing variable treatment of charter (some jurisdictions invoke principles; others suppress)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.71).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta as Feudal Artifact (Obsolescence Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '407625f4-ae9d-46c4-b634-5dab0ad3d0f5').
narrative_ontology:cs_kernel_codification('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', fixed_text).
narrative_ontology:cs_authority_grounding('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', extraction).
narrative_ontology:cs_interpretation_layer_present('407625f4-ae9d-46c4-b634-5dab0ad3d0f5').
narrative_ontology:cs_reading_relation('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', magna_carta_constraint_authority__living_constitutionalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', foundational, feudal_precedent_non_binding_modern_sovereignty).
narrative_ontology:cs_axiom_status(feudal_precedent_non_binding_modern_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', feudal_precedent_non_binding_modern_sovereignty, conventional).
narrative_ontology:cs_axiom('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', foundational, statutory_law_supersedes_charter_authority).
narrative_ontology:cs_axiom_status(statutory_law_supersedes_charter_authority, holdable).
narrative_ontology:cs_axiom_grounding('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', statutory_law_supersedes_charter_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', modern_statutory_sovereignty).
narrative_ontology:cs_drift_state('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('407625f4-ae9d-46c4-b634-5dab0ad3d0f5', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_proponents).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_litigants).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_discretion_unconstrained_by_feudal_precedent).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, modernity_renders_medieval_compacts_inoperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains that Magna Carta is a historical artifact addressing feudal relationships that no longer exist; argues modern sovereignty operates under different constitutional principles (written constitutions, statutory law, parliamentary supremacy). Administers the narrative that charter provisions are superseded by modernization and irrelevant to executive authority. Claims Magna Carta is invoked theatrically by constitutionalists but carries no enforceable weight in contemporary governance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_power_seat, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Argue that Magna Carta represents an inherited principle of lawful restraint on executive power; seek to invoke it as a living constitutional source. Their exit from the constraint is blocked because the executive's obsolescence framing denies them standing to cite the charter in litigation or constitutional discourse. They bear the cost of reduced restraint on executive authority and have limited recourse to pre-modern constitutional sources.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates, payer,
    organized, generational, constrained, national).

% Advocate for constitutional limits on power grounded in historically-rooted legal principles. The obsolescence reading forecloses their argument by treating Magna Carta as historically dead weight rather than a living precedent. They face suppression through narrative exclusion: courts dismiss charter invocations as anachronistic rather than engaging the underlying claim of lawful restraint.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_proponents, payer,
    moderate, generational, constrained, national).

% Seek due process protections and claim common-law rights that trace to Magna Carta precedent. Under the obsolescence reading, courts treat such claims as historically inoperative, forcing litigants to argue only under modern statutory law and written constitutions. They lose an argumentative vector for restraint on state power and must accept whatever statutory protections the executive-controlled legislative process provides.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_litigants, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, common_law_litigants, beneficiary).

% Scholars who argue for charter continuity (living constitutionalism, inherited restraint) are marginalized in policy discourse when the obsolescence frame dominates. Their research agenda is delegitimized as historical nostalgia rather than juridical analysis. They remain outside the executive-administrative framing but lack institutional power to contest it in real-time governance.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, academic_constitutionalists, excluded,
    moderate, generational, constrained, national).

% Examines how different constitutional democracies invoke or suppress medieval charters as living restraints (Canada, Australia, South Africa invoke Magna Carta principles; U.S. common-law argument faces obsolescence framing in federal courts). No direct stake but provides external reference for whether the obsolescence claim is universal or reading-contingent.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, comparative_constitutional_systems, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_power_seat).
narrative_ontology:fixing_cost_class(magna_carta_constraint_authority__feudal_obsolescence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None claimed in this reading. The obsolescence reading treats Magna Carta as a defunct feudal compact with no modern coordination function—it does not solve a present-day collective problem.
% TRANSFER_FUNCTION: The arrangement moves rhetorical authority (the right to cite Magna Carta as a constitutional source) FROM popular constitutionalists and juridical-restraint advocates TO the executive power seat, which monopolizes the authority to declare when historical precedents are 'operative' vs. 'obsolete.'
% ABSENT_VOICES: Living constitutionalists and common-law scholars who argue charter continuity are structurally excluded from setting the interpretive frame—they can object but their objections are preemptively categorized as anachronistic rather than engaged on the merits.
% DISAPPEARANCE_RATIONALE: If the obsolescence reading disappeared—if courts and executives reverted to treating Magna Carta as a living constitutional source—executive power would face inherited restraints on arbitrary action, due process claims would gain textual grounding, and the balance of authority between executive and litigant would shift. The executive-power seat contests this: it argues Magna Carta's disappearance from operative doctrine caused no rearrangement, only clarified what was always true (feudal precedent does not bind modern sovereignty). The disagreement is structurally fundamental.
% FOUNDING_PROBLEM: The founding problem, from this reading's internal logic, is the mistake of treating a 13th-century baronial compact as though it retained authority over modern legal systems. The 'problem' solved by the obsolescence reading is clearing away competing jurisdictional claims and aligning constitutional doctrine with modern state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The executive establishment, legal positivists, and modernist constitutional scholars attest that feudal precedent should not constrain modern sovereignty. Living constitutionalists, common-law advocates, and comparative constitutionalists outside the executive-sovereignty frame attest that charter principles remain operative and that the 'obsolescence' claim is a narrative choice, not a structural fact. No consensus exists; the corroboration splits along reading lines.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading is classified as PITON because: (1) the constraint's primary function (restraint on arbitrary executive action) has atrophied—the charter is no longer a working restraint in most modern jurisdictions; (2) what remains is mostly performative—invocation persists in scholarly and litigant discourse but is suppressed at the institutional level; (3) no party benefits enough to maintain it (no concentrated beneficiary) and no party is hurt enough to fix it (suppression is diffuse, affecting intellectuals and common-law advocates rather than a unified constituency with power to change doctrine). The theater_ratio of 0.62 reflects this performative maintenance: a substantial share of charter discourse is ritual invocation by constitutionalists who know they will lose, rather than functional restraint on power. Extractiveness of 0.68 captures the asymmetry: the constraint enables executive discretion by foreclosing restraint arguments, and this asymmetry is actively defended through judicial suppression and narrative marginalization. Suppression of 0.71 reflects both structural barriers (standing doctrine, res judicata rules) and internalized skepticism (legal professionals self-censor from charter arguments). The accessibility_collapse of 0.48 is lower than mountains because alternatives (statutory rights, written constitutions) do exist—they are not ideal substitutes for common-law precedent but are available, creating partial collapse rather than total foreclosure. Resistance of 0.74 is high because living constitutionalists, common-law scholars, and comparative constitutional advocates actively resist the obsolescence frame and argue for charter continuity—but their resistance remains institutional and discursive, not powerful enough to reverse suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the executive-power seat, the obsolescence reading is accurate doctrine: medieval feudal precedent should not constrain modern sovereignty, and courts are correctly applying modernist constitutional principles. From this seat, the constraint is not extractive at all—it is the proper application of legal positivism and sovereignty theory. From the popular-constitutionalist seat, the same structure is a snare dressed as doctrine: the executive-power seat has claimed interpretive authority over which precedents are operative, uses that authority to suppress restraint arguments, and maintains the suppression through repeated judicial dismissal. These are not mere disagreements; they instantiate different structural relationships to the same constraint. The engine computes both: executive_seat perceives cooperation (doctrine is correctly applied), payer_seats perceive extraction (restraint is suppressed). This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive power seat sits at d ≈ 0.1 (full beneficiary): it collects maximal discretion from the constraint, faces no exit costs from the obsolescence frame, and administers the authority structure that maintains it. Popular constitutionalists sit at d ≈ 0.9 (full target): they are structurally prevented from invoking restraint arguments, bear the cost of reduced precedent access, and exit is blocked by suppression doctrine. Juridical restraint proponents sit at d ≈ 0.85: their entire argumentative strategy is foreclosed by the obsolescence reading. Common-law litigants sit at d ≈ 0.95 (trapped targets): they have no alternative restraint mechanism and are forced into statutory reliance. This asymmetry is the core of the extractive structure: one seat (executive) monopolizes authority to declare precedents operative or obsolete, and uses that authority to strip away restraint arguments available to other seats. The directionality computation amplifies extraction for these highly-targeted seats because they are both trapped (no alternative exit) and identity-locked (as lawyers, constitutionalists, they cannot simply abandon the argument).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (a constraint whose mandate has outlived its function) is HIGH on this reading. The founding mandate was to restrain feudal lords' arbitrary authority—a problem that was solved by the transition to nation-state legal systems and statutory law. Yet the constraint persists in doctrine as a ritualistic invocation with minimal restraint force. The measurement series captures the mandatrophy accumulation: at 1215 (t=0), Magna Carta had a live mandate (feudal restraint); at 1600 (t=385), the feudal problem was gone but the charter persisted; by 1980 (t=765), invocation is mostly theatrical (theater_ratio 0.54). By 2025, theater_ratio has risen to 0.62, indicating mandatrophy is deepening—the constraint is maintained through narrative and institutional habit rather than functional necessity. The six_questions.founding_problem_status='contested' reflects this: the executive-power seat claims the founding problem was a historical mistake (feudal precedent should never have applied to modern states), while constitutionalists claim the founding problem (arbitrary authority) remains live. This reading supports the mandatrophy verdict: the constraint is a zombie that persists because no single party has the power to formally abolish it, but all parties except the executive-power seat would prefer to see restraint mechanisms that actually work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contention,
    'Is Magna Carta a feudal artifact with no modern authority (obsolescence reading), or a living constitutional source that binds all subsequent rulers through inherited restraint (living constitutionalism reading)?',
    'No empirical resolution possible—this is a committer-axis disagreement about which reading the SAME KERNEL instantiates. Resolution requires accepting one normative framework over another: either modernist legal positivism or common-law continuity.',
    'If living constitutionalism prevails, the constraint type shifts from piton to tangled_rope (real coordination function + asymmetric extraction); if obsolescence prevails, it remains piton with high theater ratio. The ε value stays constant (the feudal compact is the fixed referent); the interpretation shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Fundamental disagreement about whether Magna Carta is an operative constitutional source or a historical precedent.').

omega_variable(
    theater_ratio_trend_interpretation,
    'Does the rising theater_ratio from 0.62 in modern times reflect performative maintenance of a dead precedent (supporting piton classification), or does it reflect increasing rhetorical struggle by living constitutionalists to invoke the charter against executive suppression (supporting snare classification)?',
    'Discourse analysis of how Magna Carta is invoked in recent litigation and constitutional commentary: (a) if invocations are dismissed as anachronistic without substantive engagement, theater-for-maintenance is supported; (b) if invocations face explicit suppression and judicial resistance, snare-with-active-enforcement is supported.',
    'Theater maintenance suggests the constraint persists through institutional inertia (piton); active suppression of restraint claims suggests the constraint persists through enforcement of executive discretion (snare). Piton is lower-harm; snare is higher-harm with identifiable victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_trend_interpretation, empirical, 'Whether rising theater reflects maintenance of a dead precedent or enforcement of extracted discretion.').

omega_variable(
    sibling_reading_structural_relationship,
    'Do the living_constitutionalism_reading and parliamentary_sovereignty_reading logically foreclose this obsolescence reading, or do they coexist as different readings of the same contested kernel?',
    'Judicial and legislative history: (a) if courts explicitly reject charter authority via statute or doctrinal shift, foreclosure is operative; (b) if courts permit charter invocation in some contexts while denying it in others (oscillating per forum), coexistence is the actual pattern.',
    'Foreclosure would mean one reading has won institutional authority and the others are superseded (historical, not current). Coexistence means all three remain live positions in the same constitutional system, held by different parties and forums—a deeper structural indeterminacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_relationship, empirical, 'Whether sibling readings are foreclosed or coexist as live positions.').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Is the measured suppression (0.71) structural—external barriers to charter invocation (standing doctrine, judicial dismissal)—or internalized—scholars and advocates have been convinced the charter is anachronistic and self-censor from invoking it?',
    'Post-suppression-removal scenario: if a constitutional amendment or statute explicitly revived Magna Carta authority, would advocates immediately resume citing it, or would internalized skepticism persist?',
    'If structural, removing barriers (judicial rule changes, new statute) would restore charter authority rapidly. If internalized, the constraint persists even after formal barriers fall—the suppression is carried in the minds of legal professionals. This affects the trajectory for remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Whether suppression of charter authority is external or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1215, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mgc_feud_tr_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1215, 0.0).
narrative_ontology:measurement_basis(mgc_feud_tr_t1215, projected).
narrative_ontology:measurement(mgc_feud_tr_t1600, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement_basis(mgc_feud_tr_t1600, observed).
narrative_ontology:measurement(mgc_feud_tr_t1750, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1750, 0.18).
narrative_ontology:measurement_basis(mgc_feud_tr_t1750, observed).
narrative_ontology:measurement(mgc_feud_tr_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement_basis(mgc_feud_tr_t1900, observed).
narrative_ontology:measurement(mgc_feud_tr_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1980, 0.54).
narrative_ontology:measurement_basis(mgc_feud_tr_t1980, observed).
narrative_ontology:measurement(mgc_feud_tr_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2025, 0.62).
narrative_ontology:measurement_basis(mgc_feud_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mgc_feud_be_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1215, 0.0).
narrative_ontology:measurement_basis(mgc_feud_be_t1215, projected).
narrative_ontology:measurement(mgc_feud_be_t1600, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement_basis(mgc_feud_be_t1600, observed).
narrative_ontology:measurement(mgc_feud_be_t1750, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1750, 0.28).
narrative_ontology:measurement_basis(mgc_feud_be_t1750, observed).
narrative_ontology:measurement(mgc_feud_be_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1900, 0.42).
narrative_ontology:measurement_basis(mgc_feud_be_t1900, observed).
narrative_ontology:measurement(mgc_feud_be_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement_basis(mgc_feud_be_t1980, observed).
narrative_ontology:measurement(mgc_feud_be_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(mgc_feud_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mgc_feud_su_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1215, 0.0).
narrative_ontology:measurement_basis(mgc_feud_su_t1215, projected).
narrative_ontology:measurement(mgc_feud_su_t1600, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1600, 0.22).
narrative_ontology:measurement_basis(mgc_feud_su_t1600, observed).
narrative_ontology:measurement(mgc_feud_su_t1750, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1750, 0.38).
narrative_ontology:measurement_basis(mgc_feud_su_t1750, observed).
narrative_ontology:measurement(mgc_feud_su_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1900, 0.52).
narrative_ontology:measurement_basis(mgc_feud_su_t1900, observed).
narrative_ontology:measurement(mgc_feud_su_t1980, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement_basis(mgc_feud_su_t1980, observed).
narrative_ontology:measurement(mgc_feud_su_t2025, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(mgc_feud_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The Magna Carta kernel decomposes into three constraint stories, one per reading (feudal_obsolescence, living_constitutionalism, parliamentary_sovereignty). Each reading instantiates a different constraint with different ε values (referent is fixed—the feudal compact—but readings differ on whether it is operative), different stakeholder structures, and different types. The feudal-obsolescence reading treats the charter as inoperative (piton with high theater); the living-constitutionalism reading treats it as operative (tangled_rope, coordination + asymmetric extraction); the parliamentary-sovereignty reading treats it as absorbed into statute (rope, pure coordination). All three remain live readings held by different institutional actors. Network edges link them: each reading influences the others by setting the interpretive frame that the others must contest within.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
