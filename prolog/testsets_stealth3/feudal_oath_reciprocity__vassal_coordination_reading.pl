% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath as Fixed Bounded Reciprocity Enforced by Charter Text (Vassal Coordination Reading)
 *   domain: medieval political economy / legal history / institutional analysis
 *
 * SUMMARY:
 *   After Carolingian public authority fragmented, western European political
 *   order came to rest on a web of acts of homage: a vassal kneels, swears
 *   fidelity, and receives a fief; a charter (or notitized oath) fixes what
 *   each side owes — defined knight service and counsel against protection,
 *   maintenance, adjudication, and capped incidental payments — and routes
 *   breaches to identifiable courts rather than to feud. This story authors
 *   ONE reading of that arrangement: the vassal_coordination_reading, on
 *   which the oath is a genuine coordination mechanism with mutual
 *   enforceability and no structural victim among its parties. Per the
 *   epsilon-referent rule, epsilon is authored for the standing oath-charter
 *   arrangement as THIS reading assesses it: fixed, bounded, reciprocal —
 *   hence low. The sibling readings (lord_extraction_reading: the oath
 *   authorizes maximal extraction bounded only by vassal capacity;
 *   ecclesiastical_mediation_reading: the oath is bound by charity and
 *   sacramental obligation) are separate constraints in separate files with
 *   their own epsilon, beneficiaries, and victims; they are linked, not
 *   averaged, here. Time mapping: T=0 corresponds to circa 850 CE
 *   (Carolingian dissolution), T=450 to circa 1300 CE (high-medieval
 *   monetization and royal-bureaucratic expansion); measurements are
 *   historiographic reconstructions from cartularies, court rolls, and
 *   formula books, marked observed.
 *
 * KEY AGENTS:
 *   - vassal_tenants: co-principal party (moderate power / constrained exit) — exchanges defined, heritable service for protected tenure; bears real priced obligations and holds real enforceable shields
 *   - landholding_lords: co-principal and administrator (powerful / constrained exit) — grants fiefs, convenes the honor court, leads the host, and is bound by the same charter texts it issues
 *   - royal_crown: apex guarantor (institutional / constrained exit) — hears breach appeals, making lower-court enforcement credible, and converts the charter order into extending royal jurisdiction
 *   - ecclesiastical_witnesses: guarantor-adjacent seat (institutional / mobile exit) — witnesses homage, supplies spiritual sanction and scriptorial archive, sets no terms and collects no charter dues
 *   - unfree_peasant_majority: excluded non-party (powerless / trapped exit) — cultivates the same estates, appears in charters as appurtenance, enjoys none of the reciprocity
 *   - institutional_historians: analytical observer (analytical / analytical exit) — reconstructs stipulated terms versus collected dues; holds no position in the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.45).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath as Fixed Bounded Reciprocity Enforced by Charter Text (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval political economy / legal history / institutional analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0').
narrative_ontology:cs_kernel_codification('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', formalized).
narrative_ontology:cs_authority_grounding('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', practice).
narrative_ontology:cs_interpretation_layer_present('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0').
narrative_ontology:cs_reading_relation('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', foundational, charter_text_fixes_reciprocal_obligations).
narrative_ontology:cs_axiom_status(charter_text_fixes_reciprocal_obligations, holdable).
narrative_ontology:cs_axiom_grounding('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', charter_text_fixes_reciprocal_obligations, conventional).
narrative_ontology:cs_axiom('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', foundational, mutual_court_enforceability_caps_seigneurial_demand).
narrative_ontology:cs_axiom_status(mutual_court_enforceability_caps_seigneurial_demand, holdable).
narrative_ontology:cs_axiom_grounding('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', mutual_court_enforceability_caps_seigneurial_demand, conventional).
narrative_ontology:cs_reference_frame('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', charter_bounded_reciprocal_service).
narrative_ontology:cs_drift_state('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', late_thirteenth_century, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1e6dcfbd-5da2-4e8c-aee1-2e90b2e30ca0', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassal_tenants).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, landholding_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, rural_peasant_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, royal_crown).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, vassal_tenants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold fiefs in return for fixed knight service, garrison duty, counsel, and capped customary payments written into their charters of homage. Receive the lord's protection, maintenance, and access to his court for disputes. Renouncing homage forfeits the fief; transferring to another lord requires the current lord's consent and fresh investiture, so exit is legally possible but costly. Service terms run with the fief to heirs. Honor culture fuses fidelity with standing — a vassal who defects carries reputational ruin — though defection and litigation nonetheless occur at real rates.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassal_tenants, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassal_tenants, payer).

% Grant fiefs, convene the honor court, distrain for breach, and lead the host. Their own charters commit them to protection, defined aid limits, and due process for their men: a lord who flouts his own text faces tenant defection, appeal to the overlord's court, and loss of able fighting men. They collect service, reliefs, and aids, and recycle them into the protection and adjudication the bargain promises. Their social standing is constituted by the charter order they administer; they cannot exit it without ceasing to be lords.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, landholding_lords, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, landholding_lords, beneficiary).

% Guarantees the last-resort forum: vassals appeal seigneurial injustice to royal courts, and the crown's willingness to hear breach cases is what makes lower charter enforcement credible. In exchange the crown accumulates caseload, precedent, fees, and a claim to superiority over all landholders — the charter order extends royal jurisdiction without royal armies. The crown cannot opt out of the order without surrendering the judicial reach it is building on it.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, royal_crown, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, royal_crown, beneficiary).

% Bishops and abbots witness acts of homage, consecrate oath-sworn instruments, and run the scriptoria where charters are copied and archived. They lend spiritual sanction to performance and archival memory to proof. Under this arrangement they neither set its terms nor collect its dues; their sanction is invoked by the parties rather than administered by them, and their houses operate across polities, unhitched from any single lordship.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_witnesses, observer,
    institutional, generational, mobile, continental).

% Cultivate the demesne and pay labor services, tallages, and milling and baking fees on the same estates the charter order organizes. Charters list them as appurtenances of the fief — 'with men, mills, and serfs' — not as parties; no clause of reciprocity runs to them, and they have no standing in the honor court. Flight to a town or another lord's land is the only exit, hedged by pursuit, return fines, and the practical difficulty of proving freedom.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, unfree_peasant_majority, excluded,
    powerless, biographical, trapped, local).

% Reconstruct the arrangement from cartularies, court rolls, pipe rolls, and formula books; compare stipulated terms with dues actually collected; track which clauses drew enforcement and which decayed into recitation. They hold no position in the arrangement and gain or lose nothing from its operation or dissolution.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, institutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__vassal_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__vassal_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In a post-Carolingian landscape with no centralized fiscal-bureaucratic state, fixes the terms of armed service, land tenure, counsel, and dispute resolution between lords and mounted elites: written charters specify what each party owes, cap incidental demands such as aids and reliefs, secure heritable tenure, and route breaches to identifiable courts instead of feud.
% TRANSFER_FUNCTION: Moves defined knight service, garrison duty, counsel, and customary payments from vassal tenants to lords; moves fiefed land, protection, maintenance, and adjudication from lords to vassal tenants — both legs bounded and recorded in charter text, with the crown skimming jurisdictional reach off the enforcement traffic.
% ABSENT_VOICES: Unfree peasants — the demographic majority of the estates involved — are not parties: they owe labor services and tallages with no charter reciprocity protecting them, and would object that the celebrated mutuality covers only the armed elite. Landless younger sons and most women (outside specific widow-dower clauses) likewise hold no seat. They are absent because the charter order legally constitutes them as objects of lordship rather than contractors; their objection survives in flight, evasion, and the occasional revolt, not in the charter record.
% DISAPPEARANCE_RATIONALE: If the oath-charter system vanished overnight, military mobilization reverts to ad hoc warband retainer paid from hand to hand, tenurial security collapses into seizure and re-seizure, and dispute resolution reverts to feud between armed households. The castle-and-honor order, the incipient legal profession living off charter disputes, and the crown's appellate jurisdiction all unravel — arrangements across the polity demonstrably depend on it.
% FOUNDING_PROBLEM: After Carolingian public authority fragmented in the ninth century, no institution could guarantee defense, order, or contract enforcement; armed magnates privatized coercion, raiding was endemic, and cultivators needed predictable protection. The oath-charter form was built to stabilize mutual relations among armed elites and to anchor a protection-for-service exchange where no state existed to impose one.
% FOUNDING_PROBLEM_CORROBORATION: Monastic annals and episcopal correspondence — institutions adjacent to the bargain but not beneficiaries of the vassal-lord exchange — attest the founding disorder directly: raiding seasons, castle warfare, and the collapse of public peace. Royal capitularies from the dissolution era attest the state-capacity vacuum from the losing side of it. Modern documentary scholarship corroborates the mechanism: fixed-term charters proliferate precisely where and when public order failed, and thin out where royal justice arrives. On the status question, no source outside the arrangement's parties certifies that the founding problem remained unsolved at interval end — royal judicial records attest its partial resolution, which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is rope because this reading's structural claim is precisely that the arrangement coordinates (fixing and bounding a land-for-service exchange that no state could organize) without a victim among its parties. The metrics describe the arrangement's operation as this reading measures it: extractiveness low (0.18) because obligations are fixed and aids capped — the residual is coordination cost, sitting just above the resource_allocation Boltzmann floor of 0.15; suppression moderate (0.45) because the arrangement's persistence requires active machinery — honor courts, distraint, forfeiture, and ultimately royal appellate force — and because exit, while legally possible, is costly (forfeiture of the fief, consent requirements for transfer); theater_ratio low (0.18) because homage ritual and charter ceremony are performative but load-bearing, with a mild late-interval rise as substance migrates to royal courts and ritual persists; accessibility_collapse moderate-low (0.35) because alternatives never fully collapsed — allodial tenure pockets, flight, commutation, and eventually cash contract persisted; resistance moderate (0.4) because vassals defied, litigated, and league-formed at real rates, the friction typical of a working rope rather than a defended construct. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: from feud-and-honor self-help (0.25) through maturing court-and-record machinery (0.45) — a rising trajectory of formalized enforcement, not a static picture. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the landholding_lord seat, the charter order is a structure he administers that caps his own discretion — a constraint he experiences as a limit he agreed to and sometimes resents; from the vassal_tenant seat, the same texts are a shield that makes his service predictable and his tenure defensible; from the royal_crown seat, the arrangement is a jurisdictional asset that extends royal justice without royal armies; from the ecclesiastical_witness seat, it is an occasion for sanction and archive; and from outside the estate gate, the unfree_peasant_majority experiences the same manorial economy as bare, unreciprocated extraction — the celebrated mutuality simply stops at the boundary of the chartered relation. Same-level lateral dynamics appear inside the vassal class: mesne tenants are lords to their own men and vassals to their superiors, deploying fixed-bounds rhetoric upward and capacity-bounds rhetoric downward, which is exactly why the sibling readings remain live within single parties' commitments. The engine computes these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: vassal_tenants and landholding_lords as direct parties, rural_peasant_communities as weak incidental beneficiaries (fixed lord-vassal obligations reduce arbitrary war-band levies crossing their lands). No victims are declared because this reading's claim is that no party bears uncompensated costs. Left to the derivation chain, declared beneficiaries would all sit near the full-beneficiary end (d near 0.1), damping effective extraction below even the modest true figure — but the derivation reads net sign, not gross flows: vassals pay substantial priced service, and lords fund protection and adjudication. Two directionality_overrides correct this: vassal_tenants (the story's only moderate-power agent) to d=0.40, near symmetric with a slight net benefit; landholding_lords (the story's only powerful agent) to d=0.32, a modest net beneficiary who also sets terms. The royal_crown (institutional) keeps its derived low d — it genuinely nets jurisdictional reach from guaranteeing the order. Rural_peasant_communities keep a weak derived beneficiary position reflecting thin coupling. No override is used as a substitute for structural declaration; the declarations stand and the overrides only adjust magnitude.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — order, defense, and contract enforcement where no state exists — remains live across nearly all of the interval, so there is no mandate outliving its function and no mandatrophy to resolve; the arrangement is doing its job at interval end, even as royal administration begins contesting exclusivity. The classification discipline matters here in both directions: reading the oath as a snare (the lord_extraction sibling's move from this seat) ignores the documented fact that vassals successfully invoked charter bounds in court and that lords who flouted their own texts lost tenants and appeals; reading it as a mountain would ignore that it is constructed, enforced, and historically bounded. The rope claim plus the charter_boundedness_effectiveness omega keeps the extraction question open on evidence rather than resolving it by fiat, and the exclusion_boundary_subsidy omega prevents the no-victim claim from silently assuming away the non-party population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (vassal_coordination_reading) of the shared kernel feudal_oath_reciprocity; the sibling readings lord_extraction_reading and ecclesiastical_mediation_reading instantiate structurally different constraints over the same oath practice. Which reading best captures the operative structure of the standing arrangement?',
    'Comparative documentary analysis: frequency of fixed-term versus discretionary clauses across charter corpora, honor-court and royal-court dockets on breach remedies, and complaint patterns in cartularies — do recorded disputes run against lordly overreach (supporting this reading) or against vassal default alone (supporting the extraction reading)?',
    'If the lord_extraction_reading better fits operative practice, this story''s low epsilon misattributes the arrangement, which is instead a tangled_rope or snare with vassal_tenants as victims; if this reading fits, the sibling stories carry inflated epsilon over a referent they describe poorly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, empirical, 'Reading-indexed classification of the shared feudal-oath kernel: which sibling reading matches operative structure.').

omega_variable(
    charter_boundedness_effectiveness,
    'Did charter text actually bind lords in practice — were stipulated aid caps honored, were breach remedies executed — or was enforcement systematically asymmetric in the lord''s favor?',
    'Compare charter-stipulated obligations with dues actually collected in pipe rolls, cartularies, and compoti; track outcomes of vassal-initiated breach actions in honor-court and royal-court records.',
    'Effective mutual binding keeps epsilon low and supports the rope classification; systematic lord-side violation converts the arrangement to a tangled_rope or snare with vassal_tenants as the victim seat, raising effective extraction sharply for the constrained-exit vassal seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_boundedness_effectiveness, empirical, 'Whether the charter''s mutual bounds were enforced symmetrically or degraded into cover for lordly extraction.').

omega_variable(
    exclusion_boundary_subsidy,
    'The no-structural-victim claim holds only within the chartered elite relation. Did the fixed, bounded vassal-lord bargain depend on shifting extraction downward onto the unfree peasantry who are outside the oath''s reciprocity?',
    'Trace whether demesne exploitation intensity and servile dues correlate with periods and regions of tightly fixed vassal obligations: did lords compensate for capped vassal-line revenues by intensifying tallages and labor services on non-party cultivators?',
    'If the charter order subsidized elite coordination through non-party extraction, this reading''s rope classification is partial and the constraint family requires a downstream peasant-extraction story linked via network.affects_constraints; if not, the boundary is stable and this story stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_boundary_subsidy, empirical, 'Whether elite reciprocity was financed by extraction from populations excluded from the oath.').

omega_variable(
    guarantor_framing_underdetermination,
    'Is the operative guarantor of the oath the charter text plus court enforceability (this reading''s framing), or the sacramental peril of perjury with charters as mere memoranda (the ecclesiastical_mediation_reading''s framing)? The same instruments support both framings.',
    'Examine which sanction contemporaries actually invoked in breach crises: court process and distraint, or compurgation, excommunication, and relic-oath theology; weigh formula-book language against docket outcomes.',
    'If sacramental guarantee is primary, this reading loses its independence from the ecclesiastical sibling, its authority_grounding shifts toward lineage-mediated religious authority, and its classification converges with the ecclesiastical story rather than standing as a distinct secular-coordination constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guarantor_framing_underdetermination, conceptual, 'Framing under-determination: charter-court guarantor versus sacramental guarantor of the same oath instruments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 0, 450).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_vcr_tr_t0, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feudal_oath_vcr_tr_t75, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(feudal_oath_vcr_tr_t150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(feudal_oath_vcr_tr_t225, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 225, 0.12).
narrative_ontology:measurement(feudal_oath_vcr_tr_t300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 300, 0.14).
narrative_ontology:measurement(feudal_oath_vcr_tr_t375, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 375, 0.16).
narrative_ontology:measurement(feudal_oath_vcr_tr_t450, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 450, 0.18).

% Extraction over time
narrative_ontology:measurement(feudal_oath_vcr_be_t0, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(feudal_oath_vcr_be_t75, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 75, 0.2).
narrative_ontology:measurement(feudal_oath_vcr_be_t150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 150, 0.18).
narrative_ontology:measurement(feudal_oath_vcr_be_t225, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 225, 0.17).
narrative_ontology:measurement(feudal_oath_vcr_be_t300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 300, 0.16).
narrative_ontology:measurement(feudal_oath_vcr_be_t375, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 375, 0.17).
narrative_ontology:measurement(feudal_oath_vcr_be_t450, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 450, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_vcr_su_t0, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(feudal_oath_vcr_su_t75, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 75, 0.28).
narrative_ontology:measurement(feudal_oath_vcr_su_t150, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 150, 0.32).
narrative_ontology:measurement(feudal_oath_vcr_su_t225, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 225, 0.36).
narrative_ontology:measurement(feudal_oath_vcr_su_t300, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 300, 0.4).
narrative_ontology:measurement(feudal_oath_vcr_su_t375, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 375, 0.43).
narrative_ontology:measurement(feudal_oath_vcr_su_t450, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 450, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the feudal oath' covers three structurally distinct claims that must not share one story. This file authors the vassal_coordination_reading (low epsilon, mutual boundedness, no party-victim). The lord_extraction_reading authors the same instruments as an authorization structure with vassals as victims (high epsilon); the ecclesiastical_mediation_reading authors them as charity-and-sacrament-bounded (epsilon keyed to how far spiritual limits actually restrained secular demand). Each sibling carries its own beneficiaries, victims, and claimed type in its own file; all three link here via network.affects_constraints. The upstream/downstream texture differs by pair: the extraction reading is the standing rival hypothesis against which this reading's low epsilon must be defended (see omega kernel_reading_indexicality), and the ecclesiastical reading shares the same documentary substrate, so guarantor-framing evidence moves both classifications (see omega guarantor_framing_underdetermination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__vassal_coordination_reading, moderate, 0.4).
constraint_indexing:directionality_override(feudal_oath_reciprocity__vassal_coordination_reading, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
