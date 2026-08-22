% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Bounded by Christian Charity and Sacramental Obligation (Ecclesiastical Mediation Reading)
 *   domain: political-economic/legal-historical
 *
 * SUMMARY:
 *   Between the Carolingian partition and the fiscal monarchies of the late
 *   thirteenth century, the operative constitution of Latin Europe was the
 *   performed act of homage and fealty: the vassal swears on relics, the lord
 *   accepts, and both stand under divine judgment. This story authors ONE
 *   reading of that arrangement — the ecclesiastical_mediation_reading —
 *   under which the oath's sacramental character and the demands of Christian
 *   charity place a substantive ceiling on what a lord may take, with the
 *   church holding the office of interpreter and sanctioner. On this reading
 *   the arrangement is a hybrid: it genuinely coordinates armed reciprocity
 *   where no state could, and it simultaneously routes interpretive
 *   authority, tithes, and court business to the clergy while capping lordly
 *   extraction. KEY AGENTS (by structural relationship):
 *   ecclesiastical_hierarchy — agenda-setting beneficiary
 *   (institutional/arbitrage), interprets charity and wields sanction;
 *   oath_recourse_vassals — beneficiary-payer (organized/identity_locked),
 *   owes service, holds recourse; extraction_capped_lords — primary payer
 *   (powerful/constrained), extraction capped, legitimation collected;
 *   tithe_bearing_peasantry — payer (powerless/trapped), bears tithes and
 *   residual renders with marginal shelter; extra_sacramental_communities —
 *   excluded (moderate/trapped), outside the oath economy entirely;
 *   legal_historians — analytical observer seeing the full structure. Sibling
 *   readings (lord_extraction_reading, vassal_coordination_reading) are
 *   separate constraints with their own epsilon values and are not averaged
 *   here.
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy: agenda-setting beneficiary (institutional/arbitrage) — declares charity's content, wields excommunication and interdict, collects tithes, fees, and interpretive authority
 *   - oath_recourse_vassals: beneficiary-payer (organized/identity_locked) — owes mounted service under relic-oath, holds lawful defiance and ecclesiastical recourse against violating lords
 *   - extraction_capped_lords: primary payer (powerful/constrained) — extraction capped by sacramental limit, collects sacral legitimation and fidelity enforcement from the same order
 *   - tithe_bearing_peasantry: payer (powerless/trapped) — bears tithe and residual renders, shielded only at the margin, no seat where limits are set
 *   - extra_sacramental_communities: excluded (moderate/trapped) — ineligible to swear, no juridical seat, objection structurally silenced
 *   - legal_historians: analytical observer — reconstructs practiced versus formulaic limits across the full interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.7).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Bounded by Christian Charity and Sacramental Obligation (Ecclesiastical Mediation Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "political-economic/legal-historical").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '695b59f3-10ff-4249-80f7-3ec64b9fc5df').
narrative_ontology:cs_kernel_codification('695b59f3-10ff-4249-80f7-3ec64b9fc5df', distributed).
narrative_ontology:cs_authority_grounding('695b59f3-10ff-4249-80f7-3ec64b9fc5df', lineage).
narrative_ontology:cs_interpretation_layer_present('695b59f3-10ff-4249-80f7-3ec64b9fc5df').
narrative_ontology:cs_reading_relation('695b59f3-10ff-4249-80f7-3ec64b9fc5df', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('695b59f3-10ff-4249-80f7-3ec64b9fc5df', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('695b59f3-10ff-4249-80f7-3ec64b9fc5df', foundational, oath_places_parties_under_divine_judgment).
narrative_ontology:cs_axiom_status(oath_places_parties_under_divine_judgment, holdable).
narrative_ontology:cs_axiom_grounding('695b59f3-10ff-4249-80f7-3ec64b9fc5df', oath_places_parties_under_divine_judgment, theological).
narrative_ontology:cs_axiom('695b59f3-10ff-4249-80f7-3ec64b9fc5df', foundational, charity_caps_lordly_demand).
narrative_ontology:cs_axiom_status(charity_caps_lordly_demand, holdable).
narrative_ontology:cs_axiom_grounding('695b59f3-10ff-4249-80f7-3ec64b9fc5df', charity_caps_lordly_demand, deontological).
narrative_ontology:cs_axiom('695b59f3-10ff-4249-80f7-3ec64b9fc5df', secondary, ecclesiastical_release_from_violated_oath).
narrative_ontology:cs_axiom_status(ecclesiastical_release_from_violated_oath, holdable).
narrative_ontology:cs_axiom_grounding('695b59f3-10ff-4249-80f7-3ec64b9fc5df', ecclesiastical_release_from_violated_oath, conventional).
narrative_ontology:cs_reference_frame('695b59f3-10ff-4249-80f7-3ec64b9fc5df', sacramental_charity_bounded_reciprocity).
narrative_ontology:cs_drift_state('695b59f3-10ff-4249-80f7-3ec64b9fc5df', high_medieval_fiscal_monarchy_turn, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('695b59f3-10ff-4249-80f7-3ec64b9fc5df', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, oath_recourse_vassals).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extraction_capped_lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tithe_bearing_peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, oath_recourse_vassals).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_binding_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canonical_jurisdiction_over_oaths).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares what charity requires, hears broken-oath cases in canon courts, and wields excommunication and interdict as the enforcement backstop beneath every feudal oath. Collects tithes, court fees, and the authority premium of being the interpreter all parties must petition. Expends real resources maintaining the machinery and spends legitimacy capital each time a sanction fails against a crowned head; can shift patronage and pressure between rival kingdoms when one realm turns hostile.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, beneficiary).

% Owes mounted service and counsel under oath sworn on relics; in exchange holds a fief and a recourse: when a lord violates the bond by seizing beyond custom or denying justice, the vassal may defy (diffidatio) and appeal for ecclesiastical sanction without forfeiting honor, because the oath binds the lord too. Renouncing homage would destroy standing, land, and identity, so exit from the frame is unthinkable rather than merely costly. Pays for the recourse in service owed and in the tithe that funds the court that hears him.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, oath_recourse_vassals, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, oath_recourse_vassals, payer).

% Holds demesne, banal rights, and armed followers, and would otherwise price protection and justice at what the market of coercion bears. The sacramental bounding caps that price: taking beyond charity exposes the lord to excommunication, releases his vassals, and invites neighbors to treat him as an oath-breaker. He pays in forgone extraction and in deference to clerical courts, while collecting legitimation and vassal-fidelity enforcement from the same sacramental order he resents. He cannot exit Christendom; his resistance runs through investiture politics and royal protection instead.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extraction_capped_lords, payer,
    powerful, generational, constrained, regional).

% Works demesne and pays render to the lord and tithe to the parish. The charity bounding shelters them at the margin — famine-year takings restrained, sanctuary respected, the theory that the lord may not take the plough-ox — but they hold no seat where the limits are defined and no court that hears them cheaply. Bound to the land, they cannot relocate away from either exaction; village collective action and flight to towns are the only levers, both costly and episodic.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tithe_bearing_peasantry, payer,
    powerless, generational, trapped, local).

% Jewish communities and other groups outside the baptismal economy cannot swear the Christian oath and so cannot hold fief, sit as vassals, or invoke its protections; they lend, practice medicine, and keep tolls at the system's edge under precarious charters. They would contest the claim that the oath order is the neutral frame of obligation — for them it is a wall — but they have no seat in synod, chapter, or court where charity's limits are set, and expulsion hangs over any open objection.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extra_sacramental_communities, excluded,
    moderate, generational, trapped, regional).

% Reads charters, cartularies, penitentials, and conciliar acta across the whole interval; reconstructs what the charity bounding actually demanded in practice versus in formula, and tracks where sanction bit and where it was performance. Holds no stake in the arrangement and can see the full structure across all seats at once.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__ecclesiastical_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures military service, counsel, and dispute resolution between armed parties of unequal strength in the absence of a state monopoly on violence; the ecclesiastical layer supplies a third-party adjudicator whose sanction — divine judgment — is credible precisely because no contending party controls it.
% TRANSFER_FUNCTION: Moves mounted service, advice, and agricultural surplus from vassals and peasants toward lords; moves tithes, court fees, and interpretive authority toward the clergy; the charity bounding sets a ceiling on the first flow and a floor under the second.
% ABSENT_VOICES: Those outside the sacramental economy — Jewish communities, heretics, and groups ineligible to swear — would object that the oath order is a wall rather than a neutral frame, and they are structurally kept out of synod, chapter, and court. The peasantry's own understanding of obligation also rarely enters the record; their objection survives only in the margins of manorial disputes and sanctuary cases.
% DISAPPEARANCE_RATIONALE: If the sacramental bounding vanished overnight, vassal recourse against violating lords collapses into private war; lordly extraction becomes bounded only by vassal capacity and revolt risk; the church loses its adjudicative lever and with it a principal channel of authority and revenue; sanctuary, truce institutions, and the dualism of canon and secular courts — much of the distinctive shape of medieval political order — rearrange around whatever enforcement the rising royal courts can supply.
% FOUNDING_PROBLEM: After the Carolingian partition, no central authority could enforce commitments between armed unequals: lords needed credible vassal fidelity, vassals needed credible protection and restraint, and private violence was unchecked. The oath, made sacramentally binding and ecclesiastically adjudicable, was built to solve this credible-commitment problem under statelessness.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Carolingian and post-Carolingian capitularies lament the vendetta and disorder the oaths were meant to cure; Byzantine and Muslim diplomatic correspondence attests Latin political fragmentation from wholly outside the arrangement; and the lords themselves — who resist the charity limits — nonetheless demand the oath's enforcement of vassal fidelity, attesting the commitment problem while disputing the cap. The loudest surviving account of the problem is the church's own, which is why the external attestations matter.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base metrics describe a hybrid at interval end. Extractiveness 0.58: the bounding genuinely caps lords (Peace-of-God-era truces, sanctuary, famine-year limits) while the church converts its adjudicative office into tithes, fees, and authority premiums, and the underlying feudal renders continue. Suppression 0.70: persistence depends on actively maintained sanction machinery — excommunication, interdict, later inquisitorial procedure — not on participant preference; suppression is authored as a raw structural property and is not scaled by anything in-story. Theater_ratio 0.42: sanction was materially binding where princes were weak and increasingly performative where fiscal monarchies could absorb interdict (Capetian France against Boniface VIII). Accessibility_collapse 0.50: exits existed at the margins — maturing royal courts, communal charters, credit networks outside the baptismal economy — but no actor inside Latin Christendom could simply opt out of the sacramental frame. Resistance 0.55: the investiture contest, baronial resistance to interdict, and royal anticlericalism were sustained, organized, and occasionally victorious. The temporal series run on one shared grid (850/950/1050/1122/1215/1300) with every tracked metric authored at every point, so no end-state value leaks backward onto earlier rows. The extraction curve is U-shaped rather than oscillatory: high during the tenth-century castle revolution when sanction was weak, dipping as the Peace of God and Gregorian reform made the limits bite, rising again as the papal monarchy converted adjudication into revenue and sanction into selective instrument — the late rise in theater_ratio is the leading edge of the arrangement outliving its enforcement credibility, not noise. Claim and metrics are independent authored facts: tangled_rope is claimed from structure (real coordination function, identifiable payers on both sides, active enforcement), and the metric values describe operation as the historiographic record shows it.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structure. From the ecclesiastical seat the arrangement is a ministry it administers — binding and loosing, a court of last resort whose sanction no contending party controls — coordination supplied at real cost. From the lordly seat the same structure is an external price control on coercion, set by a jurisdictional competitor. From the vassal seat it is recourse insurance: expensive in service and tithe, but the only forum where a weaker armed party can defy a stronger one without dishonor. From the peasant seat the charity ceiling is largely rumor at the margin of an extraction that continues; village coalition and sanctuary-seeking are the available levers, and they are episodic precisely because the exit is trapped. The engine derives these divergences from the declared positions and exits; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to structure: the church is declared beneficiary (tithes, fees, interpretive authority) and is the agenda-setter; vassals-with-recourse are beneficiaries (a forum and lawful defiance) who simultaneously pay service and tithe; lords are declared victims of the bounding (forgone extraction, deference to clerical courts) while collecting legitimation from the same order; the peasantry are victims (tithe, residual renders) with marginal shelter. Two directionality overrides correct derivations that would misread net position. Institutional -> 0.28: deriving the church's d from its beneficiary declaration alone would seat it near pure subsidy, ignoring the enforcement expenditure, the legitimacy capital burned on failed sanctions, and the organized resistance it meets. Powerful -> 0.65: deriving the lords' d from their victim declaration alone would seat them near full target, ignoring the vassal-fidelity enforcement and sacral legitimation they collect from the identical structure. Suppression is unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabels are blocked. Calling the arrangement a rope ignores the asymmetric extraction running through the same structure — the church's authority rents and the peasantry's continuing burden — which is why all three tangled_rope gates (coordination function, declared victims, active enforcement) are satisfied. Calling it a snare ignores that the coordination function was real and the caps sometimes bit hard enough to change lordly behavior. On mandatrophy: the founding problem — credible commitment between armed unequals under statelessness — stayed live across the interval, so no dead-mandate condition holds mid-interval; the arrangement begins outliving its function only as royal courts and salaried officials mature after 1200, visible as the post-1215 rise in theater_ratio and the practice_drift entry in cs_structure. At interval end the transition is underway but unresolved: theatrical maintenance growing, function not yet gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (ecclesiastical_mediation_reading) of the kernel feudal_oath_reciprocity; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Not resolvable by data alone — it is a framing choice. Resolution comes from the corpus treating each reading as its own constraint file and comparing computed classifications across the family.',
    'Under lord_extraction_reading the victim set inverts (extracted vassals and peasants become the victims; the church becomes an ally or irrelevance of the extractor) and epsilon rises sharply. Under vassal_coordination_reading epsilon falls toward rope levels and the church''s role drops out of the constraint entirely. This file''s moderate-epsilon tangled_rope profile holds only for the ecclesiastical reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: reading-indexed epsilon over a fixed referent; siblings are separate constraints, not measurement settings.').

omega_variable(
    charity_content_underdetermination,
    'What did Christian charity concretely cap — bare-subsistence takings, all unreasonable tallages, or only acts shocking to clerical sensibility?',
    'Systematic coding of penitentials, conciliar canons, and decided canon-law cases across the interval for the thresholds actually applied to lordly exaction.',
    'A narrow charity yields higher epsilon (the bounding mostly rhetorical); a broad charity yields lower epsilon and strengthens the coordination component of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charity_content_underdetermination, empirical, 'The operative content of the charity limit is underdetermined by formulaic sources.').

omega_variable(
    sanction_materiality,
    'Did excommunication and interdict materially constrain lords, or bind only where princely fiscal-military power was weak?',
    'Compare documented compliance and behavior-change episodes across regimes of differing capacity — Ottonian Germany, Capetian France, Norman Sicily — holding the sanction law constant.',
    'If sanction systematically failed against strong lords, theater_ratio is understated and the arrangement trends piton-ward earlier than the authored series suggests; if it bit broadly, the suppression machinery was doing real coordinative work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanction_materiality, empirical, 'Material efficacy of the sacramental sanction varies by regime strength.').

omega_variable(
    tithe_decomposition_boundary,
    'Is the church''s fiscal extraction (tithe, court fees, probate) part of THIS constraint''s epsilon, or a separate constraint riding on the same adjudicative authority?',
    'Per the epsilon-invariance principle this story scopes epsilon to the oath-bounding mechanism; the tithe system warrants its own story with its own beneficiaries and victims. Test by classifying the tithe separately and checking whether the family classifications remain stable.',
    'Folding tithe extraction into this epsilon would raise it materially and could tip the computed type toward snare; keeping it separate preserves the reading''s moderate-epsilon profile and keeps each story''s epsilon stable under observable choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tithe_decomposition_boundary, conceptual, 'Decomposition boundary between the oath-bounding mechanism and clerical fiscal extraction.').

omega_variable(
    sanction_internalized_vs_structural,
    'Did the sacramental threat constrain through internalized dread (confession culture, hell-fire preaching) or through structural sanction (courts, interdict, social death)?',
    'Compare conduct where confessional access and preaching intensity differed — regular versus secular clergy density, mendicant penetration after 1220 — holding sanction law constant.',
    'If largely internalized, suppression persists after institutional enforcement decays, raising effective suppression beyond the structural measure and slowing any post-1300 relaxation; if largely structural, enforcement decay translates directly into constraint decay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanction_internalized_vs_structural, empirical, 'Mechanism split between internalized and structural components of sacramental coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 843, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_emr_tr_t850, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 850, 0.18).
narrative_ontology:measurement_basis(feudal_oath_emr_tr_t850, observed).
narrative_ontology:measurement(feudal_oath_emr_tr_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 950, 0.21).
narrative_ontology:measurement_basis(feudal_oath_emr_tr_t950, observed).
narrative_ontology:measurement(feudal_oath_emr_tr_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1050, 0.26).
narrative_ontology:measurement_basis(feudal_oath_emr_tr_t1050, observed).
narrative_ontology:measurement(feudal_oath_emr_tr_t1122, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1122, 0.31).
narrative_ontology:measurement_basis(feudal_oath_emr_tr_t1122, observed).
narrative_ontology:measurement(feudal_oath_emr_tr_t1215, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1215, 0.37).
narrative_ontology:measurement_basis(feudal_oath_emr_tr_t1215, observed).
narrative_ontology:measurement(feudal_oath_emr_tr_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1300, 0.42).
narrative_ontology:measurement_basis(feudal_oath_emr_tr_t1300, observed).

% Extraction over time
narrative_ontology:measurement(feudal_oath_emr_be_t850, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 850, 0.6).
narrative_ontology:measurement_basis(feudal_oath_emr_be_t850, observed).
narrative_ontology:measurement(feudal_oath_emr_be_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 950, 0.57).
narrative_ontology:measurement_basis(feudal_oath_emr_be_t950, observed).
narrative_ontology:measurement(feudal_oath_emr_be_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1050, 0.52).
narrative_ontology:measurement_basis(feudal_oath_emr_be_t1050, observed).
narrative_ontology:measurement(feudal_oath_emr_be_t1122, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1122, 0.5).
narrative_ontology:measurement_basis(feudal_oath_emr_be_t1122, observed).
narrative_ontology:measurement(feudal_oath_emr_be_t1215, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1215, 0.55).
narrative_ontology:measurement_basis(feudal_oath_emr_be_t1215, observed).
narrative_ontology:measurement(feudal_oath_emr_be_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1300, 0.58).
narrative_ontology:measurement_basis(feudal_oath_emr_be_t1300, observed).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_emr_su_t850, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 850, 0.46).
narrative_ontology:measurement_basis(feudal_oath_emr_su_t850, observed).
narrative_ontology:measurement(feudal_oath_emr_su_t950, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 950, 0.5).
narrative_ontology:measurement_basis(feudal_oath_emr_su_t950, observed).
narrative_ontology:measurement(feudal_oath_emr_su_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1050, 0.57).
narrative_ontology:measurement_basis(feudal_oath_emr_su_t1050, observed).
narrative_ontology:measurement(feudal_oath_emr_su_t1122, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1122, 0.61).
narrative_ontology:measurement_basis(feudal_oath_emr_su_t1122, observed).
narrative_ontology:measurement(feudal_oath_emr_su_t1215, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1215, 0.66).
narrative_ontology:measurement_basis(feudal_oath_emr_su_t1215, observed).
narrative_ontology:measurement(feudal_oath_emr_su_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1300, 0.7).
narrative_ontology:measurement_basis(feudal_oath_emr_su_t1300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassal_coordination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the feudal oath' decomposes into three structurally distinct claims about the same performed act. This story (ecclesiastical_mediation_reading) authors the charity/sacrament bounding with moderate epsilon; lord_extraction_reading authors the authorization-of-extraction claim (high epsilon; victims are the extracted vassals and peasantry); vassal_coordination_reading authors the fixed-reciprocal-terms claim (low epsilon; both oath parties benefit). The upstream reading with the strongest documentary basis (vassal_coordination_reading, charter evidence) typically anchors the family; this reading influences both siblings by changing the legitimacy conditions under which extraction and charter terms are argued, without foreclosing either. Every family member links the others via network.affects_constraints; each file carries its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, institutional, 0.28).
constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
