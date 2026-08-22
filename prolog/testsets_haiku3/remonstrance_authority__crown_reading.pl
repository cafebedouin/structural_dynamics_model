% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Minoritarian Veto (Crown Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   Under the crown reading, the remonstrance right—the authority of
 *   provincial magistrates (especially the parlements) to remonstrate against
 *   and block the registration of royal edicts—is reframed as an illegitimate
 *   minoritarian veto protecting particularist privileges rather than a
 *   fundamental constitutional safeguard. The reading emerges in the 17th
 *   century as royal fiscal reformers and centralists argue that magistrates
 *   abuse remonstrance to preserve tax exemptions, monopolistic judicial
 *   fees, and local autonomy against necessary Crown initiatives. The Crown
 *   enters the victim set because its fiscal and legal authority is
 *   frustrated by magistrate obstruction; ordinary subjects enter the victim
 *   set because the preservation of magistrate privilege (through blocked
 *   reform) perpetuates economic monopolies and fragmented justice. This
 *   reading coexists with the magistrate_reading, which frames remonstrance
 *   as essential constitutional law protecting ancient liberties against
 *   arbitrary royal innovation. The constraint is CLAIMED as snare
 *   (extraction by magistrates, active enforcement via veto threat); this is
 *   the crown's own framing applied to the remonstrance mechanism. The
 *   authored metrics describe substantively extractive operation with rising
 *   theater (performance of constitutional deliberation masking privilege
 *   protection) and intensifying suppression (Crown agents increasingly
 *   punished for circumventing the veto, magistrate resistance
 *   institutionalized).
 *
 * KEY AGENTS:
 *   - Crown fiscal authority — the royal government seeking to implement tax and legal policy; frustrated by remonstrance blocking; enters victim set when thwarted
 *   - Magistrate oligarchy — provincial courts and nobles wielding remonstrance power; extract privilege preservation; principal beneficiary
 *   - Provincial subjects — ordinary population in magistrate-controlled provinces; trapped in non-enforcement of progressive policy; bear dual costs of Crown impasse and magistrate monopoly
 *   - Crown reformist faction — internal Crown agents and administrators seeking streamlined authority; excluded from remonstrance negotiation
 *   - Crown jurists — analysts of constitutional doctrine; observe and interpret the constraint from the analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.72).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional/political").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'ca7bf4b3-396a-40df-aed7-3eb85e095451').
narrative_ontology:cs_kernel_codification('ca7bf4b3-396a-40df-aed7-3eb85e095451', fixed_text).
narrative_ontology:cs_authority_grounding('ca7bf4b3-396a-40df-aed7-3eb85e095451', extraction).
narrative_ontology:cs_interpretation_layer_present('ca7bf4b3-396a-40df-aed7-3eb85e095451').
narrative_ontology:cs_reading_relation('ca7bf4b3-396a-40df-aed7-3eb85e095451', remonstrance_authority__magistrate_reading, coexists_with).
narrative_ontology:cs_axiom('ca7bf4b3-396a-40df-aed7-3eb85e095451', foundational, remonstrance_as_particularist_obstruction).
narrative_ontology:cs_axiom_status(remonstrance_as_particularist_obstruction, holdable).
narrative_ontology:cs_axiom_grounding('ca7bf4b3-396a-40df-aed7-3eb85e095451', remonstrance_as_particularist_obstruction, instrumental).
narrative_ontology:cs_axiom('ca7bf4b3-396a-40df-aed7-3eb85e095451', secondary, rational_governance_requires_executive_fluidity).
narrative_ontology:cs_axiom_status(rational_governance_requires_executive_fluidity, holdable).
narrative_ontology:cs_axiom_grounding('ca7bf4b3-396a-40df-aed7-3eb85e095451', rational_governance_requires_executive_fluidity, empirically_contingent).
narrative_ontology:cs_reference_frame('ca7bf4b3-396a-40df-aed7-3eb85e095451', sovereign_prerogative_doctrine).
narrative_ontology:cs_drift_state('ca7bf4b3-396a-40df-aed7-3eb85e095451', late_18th_century_ancien_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ca7bf4b3-396a-40df-aed7-3eb85e095451', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, magistrate_oligarchy).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, provincial_subjects_burdened_by_non_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Crown's fiscal and legal authority. It seeks to levy new taxes (military funding, administrative expansion, modernization), implement legal reforms (standardized justice procedures, anti-monopoly measures), and modernize governance structures. The remonstrance right forces it to deliberate with and negotiate around magistrate objections before implementation. When magistrates invoke remonstrance on high-value issues (property taxes, judicial reform threatening magistrate income), the Crown cannot simply override without triggering constitutional or military crisis. It absorbs the cost of fiscal impasse, delayed reform, and institutional workarounds.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_fiscal_authority, payer,
    institutional, generational, trapped, national).

% Provincial magistrates and courts (parlements, regional nobility, judicial office-holders). They use remonstrance to protect landed tax exemptions, monopolistic control of local justice (generating income from court fees and fines), trade monopolies, and regional autonomy. A remonstrance on a proposed tax on nobility blocks a major Crown revenue initiative; a remonstrance on unified justice procedure preserves the magistrate courts' exclusive jurisdiction and fee income. They frame this as constitutional protection of ancient rights; it functions as concentrated extraction of the veto power. Their exit is mobile: they can accept routine edicts and selectively remonstrate on issues touching their core privileges.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, magistrate_oligarchy, beneficiary,
    organized, generational, mobile, regional).

% Ordinary provincial populations—peasants, merchants, minor landholders, urban workers—in regions where magistrates wield remonstrance power. They experience the cost of fiscal impasse (unpaid military, degraded frontier defense) and the cost of preserved magistrate privilege (continuing feudal monopolies on local commerce, fragmented and expensive justice systems, restrictions on trade and movement). They have no seat in remonstrance deliberations. The Crown's reform initiatives (unified justice, reduction of local monopolies, more efficient taxation) would benefit them; magistrate remonstrance blocks these reforms. They are trapped—unable to emigrate, unable to influence magistrate decisions, unable to appeal to the Crown's authority directly.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_subjects_burdened_by_non_enforcement, payer,
    powerless, biographical, trapped, local).

% Crown ministers, military commanders, and progressive administrators who seek fiscal and legal modernization. They view remonstrance as an obstacle to necessary governance and see magistrate privilege preservation as feudal obstruction. They are excluded from remonstrance negotiations—magistrate councils do not include Crown reformists, and Crown agents have no formal standing to defend reform initiatives during remonstrance deliberation. They are constrained: they can influence Crown policy from within, but their preferred policies are routinely blocked by magistrate veto. Their only workaround is to argue for Crown override, which risks institutional crisis.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_reformist_faction, excluded,
    powerful, biographical, constrained, national).

% Crown legal theorists and constitutional advisors. They analyze remonstrance under the crown reading as illegitimate obstacle to sovereign prerogative and argue that the Crown's authority should not be contingent on magistrate acceptance. They author briefs against remonstrance, develop theoretical arguments for Crown supremacy, and counsel the Crown on legal strategies to circumvent or override remonstrance. They have no direct power to abolish remonstrance but shape the intellectual framework Crown policymakers use to contest it.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_jurists, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, magistrate_oligarchy).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal deliberative process through which provincial magistrates register and substantiate objections to royal edicts before implementation, enabling negotiation and preventing unilateral imposition. In principle, this separates hasty policy from vetted policy and preserves procedural legitimacy.
% TRANSFER_FUNCTION: Moves the authority to block fiscal or legal innovation from the Crown to provincial magistrates, as the price of their cooperation in registering and implementing directives. Magistrates extract the right to preserve local privilege (tax exemptions, monopoly preservation, procedural autonomy) in exchange for grudging compliance with noncontroversial orders.
% ABSENT_VOICES: Ordinary provincial subjects have no seat in remonstrance proceedings and no voice in magistrate councils; they would object to the preservation of magistrate monopolies and fiscal privilege but are structurally excluded. Reform-minded agents within the Crown are also absent — they would argue for streamlined authority but are denied standing in magistrate deliberations.
% DISAPPEARANCE_RATIONALE: If remonstrance authority vanished, the Crown could implement fiscal and legal policy unilaterally; provincial magistrates would lose their veto and their ability to preserve landed privileges; ordinary subjects would face streamlined but Crown-directed governance (or chaos, depending on Crown competence); the balance of power between central and local authority would shift decisively toward the Crown. The social and fiscal order would reorganize around centralizing authority rather than aristocratic local autonomy.
% FOUNDING_PROBLEM: Early modern kingdoms faced the coordination problem of registering royal edicts through local authorities who held real enforcement power. Without a formal remonstrance process, magistrates would simply ignore inconvenient directives, creating legal chaos; with it, magistrates gained assured input in exchange for accepting the discipline of deliberation before final defiance.
% FOUNDING_PROBLEM_CORROBORATION: Crown fiscal advisors and reform-minded administrators attest the founding problem is solved — magistrates do register most edicts and do engage in formal deliberation; they argue remonstrance now functions as privilege preservation rather than coordination. Independent constitutional historians document that by the 18th century the Crown has developed administrative and military capacity to implement policy despite magistrate resistance, rendering the coordination problem obsolete. The magistrate reading disputes this, asserting the founding problem remains live — without remonstrance, the Crown would impose arbitrary innovation on provinces without accountability; historians documenting magistrate resistance confirm the constraint remains active through magistrate defense, not coordination need.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) and rising (0.42 → 0.68 from 1620 to 1789) because the magistrate oligarchy increasingly uses remonstrance to preserve purely particularist privileges—tax exemptions, monopoly preservation, procedural autonomy—decoupled from any real coordination function. The founding problem (registering edicts through powerful local actors) is progressively solved by mid-17th century, yet remonstrance persists and hardens as a veto mechanism protecting magistrate wealth. Theater rises sharply (0.18 → 0.41) because magistrate remonstrance is increasingly dressed in constitutional language and procedural dignity while functioning as raw privilege extraction. Suppression requirement rises (0.48 → 0.72) because the Crown must invest in institutional workarounds (creating parallel authorities, threatening magistrate position, building military capacity to override) to implement policy despite the veto. The constraint is not abandoned because remonstrance carries centuries of legitimacy that make frontal abolition politically costly; instead, suppression hardens as the constraint's enforcement machinery matured. All measurements are authored on a shared single time grid: every metric is valued at every time point (1620, 1680, 1740, 1789), preventing misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The magistrate seat and the Crown seat compute radically different type classifications from the same structural data. From the magistrate seat: remonstrance is rope (genuine coordination function — edicts do require deliberation before registration, and the process does reduce chaos) with beneficiaries (magistrates) and modest extraction (the veto power, justified as constitutional). From the Crown seat: remonstrance is snare (pure extraction of veto power, coordination function is secondary theater, persistence depends on Crown not having a unilateral override mechanism yet having inherited expectations of magistrate participation). The engine computes both per-seat readings from the structural data; the authored claim (snare) reflects the crown reading's competitive thesis, not a false consensus.
 *
 * DIRECTIONALITY LOGIC:
 *   The magistrate oligarchy sits at d near the beneficiary extreme (d ≈ 0.1–0.2): they collect the right to block innovation without running the state apparatus; they hold organized power; their exit is mobile (they can accept most edicts and selectively remonstrate on high-value issues). The Crown fiscal authority sits at d near the target extreme (d ≈ 0.85–0.95): it bears the cost of blocked initiatives (fiscal shortfalls, delayed reform), possesses institutional power but is trapped (it cannot simply abolish magistrate remonstrance without triggering constitutional or military crisis), and faces compressed exit options. Provincial subjects sit at d extreme target (d ≈ 0.95–0.98): they are powerless, trapped, bear costs of both Crown impasse and magistrate monopoly, and have no exit short of emigration. The divergence across seats is structural and enforced: an ordinary magistrate perceives the arrangement as legitimate constitutional check (beneficiary frame); a Crown minister perceives it as obstruction (target frame); an ordinary subject perceives it as compounded oppression from above and below (target frame, the worst-positioned seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination between Crown and powerful local agents) was acute in 1620 when magistrate enforcement capacity was genuinely necessary for implementation. By 1789, the founding problem is dead—the Crown has developed administrative and military capacity to implement policy without magistrate cooperation, and the remonstrance right persists as pure privilege preservation despite its founding function being obsolete. The constraint exhibits mandatrophy: a mechanism built to solve a real coordination problem has outlived its justification and is now maintained as a veto protecting particularist extraction. The crown reading captures this mandatrophy explicitly by claiming snare type (persistence depends on extractive benefit to magistrates, not on coordination need); the magistrate reading denies the mandatrophy by arguing ancient constitutional principles remain live. The measurement series traces the mandate death: base_extractiveness rising while suppression requirement rises (the Crown is working harder to enforce policy despite the veto, indicating the coordination problem has shifted from 'how do we involve magistrates' to 'how do we prevent magistrates from blocking'); theater rising (constitutional language masks privilege extraction) is the signature of mandatrophy—form persists while function atrophies. The constraint is NOT classified as piton because magistrates actively defend and leverage remonstrance (not passive maintenance) and directly benefit from it (not diffuse); it is snare because extraction is concentrated in the magistrate oligarchy and requires active suppression of Crown override authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence_contested,
    'Is the founding coordination problem (registering edicts through powerful local actors) genuinely solved by 1789, or does the Crown''s dependence on magistrate legitimacy remain a live constraint even after it develops enforcement capacity?',
    'Counterfactual analysis: if the Crown attempted to enforce edicts without remonstrance registration (bypass the magistrate entirely), would resistance from magistrate and subject populations be sufficient to force reversal? Historical evidence from cases where Crown attempted such bypass (e.g., edicts of will, lit de justice proceedings) shows magistrate defiance and sometimes civil unrest, but also Crown success in military enforcement.',
    'If the founding problem is genuinely solved, the constraint is mandatrophy (piton-adjacent) — it persists purely through institutional inertia and magistrate active defense, not through any coordination need. If the founding problem remains live (magistrate legitimacy is still necessary for stable implementation), the constraint retains a coordination function even under the crown reading, and the classification softens toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_contested, empirical, 'Whether remonstrance remains a coordination necessity or has become pure privilege extraction.').

omega_variable(
    alternative_reading_coexistence,
    'Can the crown reading and the magistrate reading coherently coexist as two institutional framings of the same mechanism, or does one logically foreclose the other when applied to the same historical moment?',
    'Axiomatic comparison: the crown reading asserts magistrate remonstrance is illegitimate obstruction (axiom: rational_governance_requires_executive_fluidity); the magistrate reading asserts remonstrance is constitutionally necessary (axiom: constitutional_protections_supersede_executive_convenience). These axioms are both deontological claims about legitimacy. Within a SINGLE institutional framework (one court, one kingdom) applying one legitimacy standard, they cannot both be true. But they DO coexist across competing institutional positions (Crown jurists vs. magistrate jurists) in the historical record.',
    'If the readings foreclose each other, the constraint is a singular battle over what remonstrance legitimately IS, and one reading will eventually dominate (crown reading wins if the Crown centralizes; magistrate reading wins if magistrates retain power). If they coexist without foreclosure (different institutional seats maintaining different readings simultaneously), the constraint is a persistent structural contest, and the classification remains snare (extraction maintained through magistrate power, contested by Crown) rather than settling into a single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_coexistence, conceptual, 'Whether the two readings are logically compatible or mutually foreclosing.').

omega_variable(
    suppression_internalization_via_legitimacy,
    'The high suppression requirement (rising from 0.48 to 0.72) — is this structural suppression (the Crown building administrative workarounds, military capacity) or internalized suppression (magistrates and subjects accepting the remonstrance veto as legitimate despite losing policy contests)?',
    'Measure the gap between Crown''s preferred policy and implemented policy: if the gap persists despite Crown administrative investment, suppression is structural (external barriers, not internalized acceptance). If magistrates accept Crown override on non-core issues while holding remonstrance on core privilege, suppression is partially internalized (acceptance of the framework even when losing specific battles). Post-Revolution data (post-1789): if remonstrance veto is abandoned instantly without riot, suppression was largely structural; if subjects resist the removal of magistrate remonstrance as loss of constitutional protection, suppression was internalized.',
    'If structural: the Crown must continuously invest in enforcement machinery; if internalized: the constraint persists through habit and legitimacy belief even after external enforcement decays. Diagnosis affects remediation and stability: structural suppression is reversible through institutional redesign; internalized suppression persists after redesign and requires re-education or generational change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_via_legitimacy, empirical, 'Whether the high suppression is external constraint or internalized acceptance of the veto''s authority.').

omega_variable(
    crown_reading_as_partisan_framing,
    'Is the crown reading a legitimate structural analysis of remonstrance as extractive veto, or is it partisan framing by Crown agents seeking to justify centralizing authority?',
    'Independent assessment by historians, constitutional scholars, and economists outside both the Crown and magistrate camps. Criteria: (a) Do the measurements of extractiveness and theater track objective policy divergences (edicts blocked, magistrate privilege preserved) or rhetorical claims by Crown agents? (b) Can the magistrate reading explain the same historical data (edicts blocked, magistrate resistance, rising enforcement costs) under a different framing (magistrates preventing arbitrary innovation)? (c) Is there any reading that makes neither the crown nor the magistrate account compelling?',
    'If the crown reading is analytically sound but applies one side''s values (rational governance efficacy) as the legitimacy standard, it remains a live reading but one among multiple coherent readings of the data. If the crown reading distorts the data or ignores alternative explanations, the snare classification may be overstated. The magistrate reading faces the same audit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(crown_reading_as_partisan_framing, preference, 'Whether the crown reading is structural analysis or partisan justification for centralization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1620, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1620, remonstrance_authority__crown_reading, theater_ratio, 1620, 0.18).
narrative_ontology:measurement_basis(remo_tr_t1620, projected).
narrative_ontology:measurement(remo_tr_t1680, remonstrance_authority__crown_reading, theater_ratio, 1680, 0.28).
narrative_ontology:measurement_basis(remo_tr_t1680, observed).
narrative_ontology:measurement(remo_tr_t1740, remonstrance_authority__crown_reading, theater_ratio, 1740, 0.38).
narrative_ontology:measurement_basis(remo_tr_t1740, observed).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__crown_reading, theater_ratio, 1789, 0.41).
narrative_ontology:measurement_basis(remo_tr_t1789, observed).

% Extraction over time
narrative_ontology:measurement(remo_be_t1620, remonstrance_authority__crown_reading, base_extractiveness, 1620, 0.42).
narrative_ontology:measurement_basis(remo_be_t1620, projected).
narrative_ontology:measurement(remo_be_t1680, remonstrance_authority__crown_reading, base_extractiveness, 1680, 0.55).
narrative_ontology:measurement_basis(remo_be_t1680, observed).
narrative_ontology:measurement(remo_be_t1740, remonstrance_authority__crown_reading, base_extractiveness, 1740, 0.64).
narrative_ontology:measurement_basis(remo_be_t1740, observed).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__crown_reading, base_extractiveness, 1789, 0.68).
narrative_ontology:measurement_basis(remo_be_t1789, observed).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1620, remonstrance_authority__crown_reading, suppression_requirement, 1620, 0.48).
narrative_ontology:measurement_basis(remo_su_t1620, projected).
narrative_ontology:measurement(remo_su_t1680, remonstrance_authority__crown_reading, suppression_requirement, 1680, 0.62).
narrative_ontology:measurement_basis(remo_su_t1680, observed).
narrative_ontology:measurement(remo_su_t1740, remonstrance_authority__crown_reading, suppression_requirement, 1740, 0.69).
narrative_ontology:measurement_basis(remo_su_t1740, observed).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__crown_reading, suppression_requirement, 1789, 0.72).
narrative_ontology:measurement_basis(remo_su_t1789, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__crown_reading, 0.12).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).

% DUAL FORMULATION NOTE:
% The remonstrance_authority kernel decomposes into two structurally distinct constraints: the crown_reading (this file) frames remonstrance as an illegitimate minoritarian veto protecting magistrate privileges, instantiating snare dynamics at high extraction with rising theater; the magistrate_reading frames remonstrance as fundamental constitutional protection against arbitrary innovation, instantiating different beneficiary/victim sets and a lower ε assessment. The same remonstrance mechanism is the referent for both readings. They coexist across competing institutional positions (Crown vs. magistrate councils) and are linked via affects_constraints to enable comparative analysis. The crown_reading asserts magistrate remonstrance extracts by blocking rational fiscal/legal reform; the magistrate_reading asserts remonstrance coordinates by requiring deliberation before innovation. Neither reading is the 'true' reading — both are live contestations instantiated by real institutional actors. The engine computes per-seat classifications from each reading's structural data independently, enabling measurement of whether different institutional seats perceive the same constraint differently (which they do).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
