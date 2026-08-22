% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Interpretive Authority over TRIPS
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   Since 1995, WTO dispute panels (and until December 2019 the Appellate
 *   Body) have held binding interpretive authority over the TRIPS text,
 *   enforced through authorized suspension of trade concessions against
 *   non-compliant members. This file instantiates ONE reading of the
 *   trips_agreement_interpretive_kernel: the reading that treats binding
 *   adjudication plus retaliation-backed enforcement as the operative
 *   constraint. Per the epsilon-invariance discipline, the sibling readings
 *   (strong_exclusivity_reading, public_health_flexibility_reading) are
 *   separate files with their own epsilon, victim sets, and classifications;
 *   they are referenced only through network edges and omega variables, never
 *   averaged into this story. The epsilon referent HERE is the standing
 *   adjudicative arrangement itself - who settles textual meaning and how
 *   compliance is compelled - assessed from this reading's own lights, not
 *   the substantive standards regime the siblings contest. KEY AGENTS (by
 *   structural relationship): - major_litigant_states: Primary beneficiary
 *   and agenda-setter (institutional/constrained) - shapes precedent through
 *   repeat litigation, collects compliance and leverage -
 *   ip_rightsholder_industries: Secondary beneficiary (organized/constrained)
 *   - gains exclusivity-hardening readings - developing_country_members:
 *   Principal target with residual forum gains (moderate/trapped) - bears
 *   compliance, litigation costs, and chilled policy space -
 *   least_developed_members: Diffuse target (powerless/trapped) - absorbs the
 *   chill with no litigation capacity - generic_pharmaceutical_producers:
 *   Target with jurisdictional mobility (moderate/mobile) -
 *   retaliation_bearing_third_parties: Collateral target
 *   (moderate/constrained) - pays for retaliation it never argued -
 *   dispute_settlement_apparatus: Administrator (institutional/constrained) -
 *   caseload and doctrinal reach grow with the system -
 *   access_to_medicines_movement: Excluded voice (organized/constrained) - no
 *   seat where readings lock - academic_trade_law_community: Analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.6).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.66).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.6).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '5334cd41-b4a2-4a7d-930a-86632d60d7b4').
narrative_ontology:cs_kernel_codification('5334cd41-b4a2-4a7d-930a-86632d60d7b4', fixed_text).
narrative_ontology:cs_authority_grounding('5334cd41-b4a2-4a7d-930a-86632d60d7b4', lineage).
narrative_ontology:cs_interpretation_layer_present('5334cd41-b4a2-4a7d-930a-86632d60d7b4').
narrative_ontology:cs_reading_relation('5334cd41-b4a2-4a7d-930a-86632d60d7b4', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('5334cd41-b4a2-4a7d-930a-86632d60d7b4', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_axiom('5334cd41-b4a2-4a7d-930a-86632d60d7b4', foundational, treaty_meaning_requires_binding_adjudication).
narrative_ontology:cs_axiom_status(treaty_meaning_requires_binding_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('5334cd41-b4a2-4a7d-930a-86632d60d7b4', treaty_meaning_requires_binding_adjudication, conventional).
narrative_ontology:cs_axiom('5334cd41-b4a2-4a7d-930a-86632d60d7b4', foundational, authorized_retaliation_is_lawful_enforcement).
narrative_ontology:cs_axiom_status(authorized_retaliation_is_lawful_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('5334cd41-b4a2-4a7d-930a-86632d60d7b4', authorized_retaliation_is_lawful_enforcement, instrumental).
narrative_ontology:cs_reference_frame('5334cd41-b4a2-4a7d-930a-86632d60d7b4', member_consented_binding_adjudication).
narrative_ontology:cs_drift_state('5334cd41-b4a2-4a7d-930a-86632d60d7b4', post_appellate_body_collapse, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('5334cd41-b4a2-4a7d-930a-86632d60d7b4', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, major_litigant_states).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, ip_rightsholder_industries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_members).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, least_developed_members).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_pharmaceutical_producers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, retaliation_bearing_third_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% File the largest share of disputes, staff the legal complex, and shape precedent through repeat play. They collect favorable readings, compliance concessions from losing members, and disciplinary leverage over rivals. Since 2017 one of them has blocked appellate appointments, converting interpretive authority itself into a bargaining chip. They accept occasional losses as the price of a system that disciplines everyone else more than it disciplines them; walking out would forfeit the steering wheel.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, major_litigant_states, agenda_setter,
    institutional, generational, constrained, global).

% Ad hoc panelists, the suspended appellate body, and secretariat lawyers who administer cases and draft reports. Caseload, doctrinal reach, and career structures grew with every accepted dispute, giving the body a standing interest in expansive readings of its own mandate. Individual officials move freely between jobs; the body itself persists only while members keep referring disputes to it.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, dispute_settlement_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Patent-, copyright-, and data-heavy firms in pharmaceuticals, software, and entertainment whose market exclusivity depends on how the treaty text is read. They lobby capitals, supply legal argumentation, and gain whenever precedent hardens exclusivity. Bilateral pressure channels and forum shopping exist as alternatives but are costlier and less comprehensive than a binding multilateral backstop, so they stay invested in the system's authority.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, ip_rightsholder_industries, beneficiary,
    organized, biographical, constrained, global).

% Mid-income members with enough legal capacity to litigate occasionally and sometimes win, including against the largest traders. They pay litigation costs, absorb compliance burdens when they lose, and watch domestic regulatory space on medicines and data narrow under accumulating precedent. Leaving would mean abandoning guaranteed market access, so they remain inside and fight through coalitions and defensive litigation.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_members, payer,
    moderate, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_country_members, beneficiary).

% Members with minimal litigation capacity who rely on transition periods and collective coalitions. They experience the system mainly as rules written elsewhere that they must eventually comply with, and they plan health policy under the shadow of disputes they could never themselves bring or defend.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, least_developed_members, payer,
    powerless, generational, trapped, global).

% Manufacturers producing off-patent and compulsory-licensed medicines face challenge risk whenever precedent hardens exclusivity. Their production sites and customer markets sit in several jurisdictions and can shift faster than treaty obligations can follow, which gives them more room than government members have.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_pharmaceutical_producers, payer,
    moderate, biographical, mobile, global).

% Exporters and consumers in states that authorize or suffer retaliation. When a losing member refuses compliance, the winner suspends tariff concessions, and the resulting price increases and lost sales land on traders and buyers in both markets who never argued the underlying case.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, retaliation_bearing_third_parties, payer,
    moderate, immediate, constrained, global).

% Civil society coalitions, health NGOs, and patient groups who would contest exclusivity-hardening readings directly. They hold no seat in proceedings; amicus participation has been contested and limited. They act through political channels such as ministerial declarations and domestic licensing law, outside the room where readings become locked.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, access_to_medicines_movement, excluded,
    organized, generational, constrained, global).

% Scholars and practitioners who map precedent, document the widening distance between treaty text and applied jurisprudence, and diagnose the enforcement decay after the appellate collapse. They collect no rents from the system and bear none of its compliance burdens.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, academic_trade_law_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, major_litigant_states).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces unilateral, power-based trade retaliation with rules-based multilateral adjudication: any member, however small, obtains a forum in which alleged violations of the agreed IP-and-trade text are heard by a neutral panel, and interpretations are settled once rather than fought bilaterally forever.
% TRANSFER_FUNCTION: Moves interpretive authority over the treaty text from individual member capitals to adjudicative bodies; moves compliance costs onto losing parties; moves litigation spending from governments to the legal complex; and, through precedent, moves policy discretion over matters such as pharmaceutical patentability and licensing from national legislatures into the accumulated case law.
% ABSENT_VOICES: Access-to-medicines advocates, patient populations, and domestic legislatures whose statutes get tested in Geneva have no seat: disputes are brought only by member governments, amicus participation is limited and contested, and a legislature speaks only through its executive, which may be the very party conceding the point.
% DISAPPEARANCE_RATIONALE: If binding interpretive authority vanished overnight, IP-and-trade conflicts would revert to unilateral determination by market size: the largest traders would define the text's meaning through bilateral pressure, smaller members would lose their only forum in which they have ever beaten the large ones, and pharmaceutical standards would be set by whoever can threaten credible market closure.
% FOUNDING_PROBLEM: Pre-1995 dispute settlement was toothless: panel reports could be blocked by consensus, letting the accused bury adverse rulings, and the largest trader policed trade grievances with unilateral domestic threats. The arrangement was built to make obligations bind the strong by replacing power with procedure.
% FOUNDING_PROBLEM_CORROBORATION: Small and middle powers corroborate both halves from outside the benefiting set: African Group, CARICOM, and Latin American submissions during the 2019-2024 appellate crisis defend binding adjudication as the founding achievement while documenting that the mechanism now fails it; GATT-era negotiation records and independent trade-law scholarship independently attest the original blocking problem. The major litigant states, by contrast, attest chiefly that the system needs reform on their terms.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: compliance burdens, litigation costs, and precedent-driven narrowing of domestic policy space are real and have accumulated, but the arrangement also delivers a forum in which small members have beaten large ones - the extraction is substantial, not total. Suppression 0.66 is structural, not internalized: members are legally barred from unilateral self-help measures, and the credible threat of authorized retaliation chills regulatory experimentation (compulsory licensing, parallel import regimes) even where no case is filed; the prohibition on unilateral measures persists as law even as enforcement machinery decays. Theater 0.42: panel reports still resolve real disputes, but after the appellate collapse a growing share of activity is performative - appeals filed into a void, compliance proceedings against members who know enforcement is broken, an interim arbitration arrangement maintained as a visible substitute. Accessibility collapse 0.52: understanding the system forecloses unilateral retaliation among members, but bilateral diplomacy, regional arrangements, and political override (the 2001 ministerial declaration on health) persist as workable alternatives. Resistance 0.62: appointment blocking by a major member, coalition-driven political overrides, and reform brinkmanship are active, organized resistance from inside. The measurement series run on ONE shared time grid (points 0,5,10,15,20,25,30) with every tracked metric authored at every point; base_properties reflect the interval end (point 30). Extractiveness rises steadily as precedent accumulates and plateaus as enforcement capacity dies; theater spikes at the appellate collapse then partially recedes as interim arbitration stabilizes some function; suppression requirement climbs through the enforcement-hardening years then decays with the machinery. Coalition note: the powerless seat (least_developed_members) exercises leverage almost exclusively through coalition formation - the health-access coalition of 2001 is the paradigm case - which is why their measured resistance contribution appears in political channels rather than in filings.
 *
 * PERSPECTIVAL GAP:
 *   Every party to this arrangement is nominally a sovereign WTO member, yet the seats compute radically differently. From the major-litigant seat the arrangement is an achievement it steers: precedent compounds in its favor, compliance pressure lands on others, and even the enforcement crisis is a bargaining chip. From the developing-country seat the same structure operates as narrowing policy space purchased with litigation money it barely has, partially offset by the forum's genuine value. The retaliation-bearing third-party seat experiences pure collateral cost - tariffs rise on goods it never disputed. The apparatus seat carries a mild self-benefit gradient: its jurisdiction and staffing grow with expansive readings. Differentiated exit options drive the divergence: trapped members cannot leave without losing market access, mobile producers can relocate, and the great powers are constrained not by weakness but because exit would surrender the steering wheel.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (major_litigant_states, ip_rightsholder_industries) place those seats near the subsidized end; victim declarations (developing_country_members, least_developed_members, generic_pharmaceutical_producers, retaliation_bearing_third_parties) place them near the target end, amplified by trapped exits for the governmental victims and damped by mobility for the generic producers. developing_country_members carry a secondary beneficiary role because the forum itself is worth something to them - they have won cases no bilateral channel would ever have given them - which moderates their derived directionality below full-target. No directionality_overrides are authored, deliberately: overrides key on the power atom, and this story has multiple distinct seats sharing each atom (institutional covers both the great-power litigants and the apparatus; moderate covers developing members, generic producers, and retaliation bearers). Any atom-keyed correction would misfire across structurally opposed seats, so per-seat differentiation is carried entirely by the beneficiary/victim declarations, the secondary role, and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a pure coordination rope would erase the documented capture asymmetry: repeat players win more, precedent compounds toward their preferred readings, and enforcement costs spill onto bystanders. Classifying it as a pure extraction snare would erase the real coordination good: small members obtained a forum in which power arguments are inadmissible, and the pre-1995 unilateral alternative was worse for nearly everyone except the single largest trader. The tangled-rope classification holds both truths and forces the analysis to locate the boundary between them - which is exactly where the live contest sits. On obsolescence: the founding problem (make obligations bind the strong) is contested rather than dead - the underlying power asymmetry persists, but the mechanism built for it is degraded, and the temporal series shows the signature of a mandate drifting toward theatrical maintenance (theater ratio spiking to 0.46 at the appellate collapse). If enforcement is never restored, the arrangement completes the drift toward inertia: administered, performed, and no longer deciding anything that market size does not already decide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_lock_direction,
    'Will accumulated panel and interim-arbitration precedent lock in the exclusivity-hardening sibling reading or the flexibility-preserving sibling reading as operative law?',
    'Track holdings on patentability thresholds, compulsory-licensing conditions, and data-exclusivity scope across the next decade of reports; code each holding against the two sibling readings'' core premises.',
    'Determines which sibling this meta-constraint entrenches: exclusivity-locking precedent raises effective extraction on health-policy space dramatically; flexibility-locking precedent reverses the direction and validates the founding bargain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_lock_direction, empirical, 'Direction of precedent accumulation between the two sibling readings.').

omega_variable(
    appellate_collapse_reversibility,
    'Is the appellate collapse a temporary bargaining pause or a permanent regime change in which bilateral power dynamics substitute for multilateral adjudication?',
    'Observe whether appointments resume, whether interim-arbitration membership grows or stagnates, and the ratio of appeals-filed-into-the-void to resolved appeals over the next five years.',
    'If permanent, the binding-interpretive core of this reading decays into performance, the theater ratio continues climbing, and the effective constraint becomes raw market power wearing procedural dress; if reversed, the arrangement recovers its adjudicative function and the extraction trajectory flattens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_collapse_reversibility, empirical, 'Whether the enforcement-decay trajectory is cyclical or terminal.').

omega_variable(
    retaliation_deterrence_vs_deadweight,
    'Does authorized retaliation actually induce compliance, or does it primarily impose deadweight losses on third-party traders while failing to change the target''s behavior?',
    'Comparative compliance-rate analysis before and after retaliation authorization across the case record, plus welfare accounting of the major retaliation episodes (banana disputes, gambling services, aircraft subsidies).',
    'If deterrence fails, the enforcement leg of this reading loses its instrumental grounding, the coordination claim weakens toward cover story, and the arrangement''s classification slides toward extraction sustained by threat rather than by function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_deterrence_vs_deadweight, empirical, 'Whether the retaliation mechanism works as enforcement or as pure cost imposition.').

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the trips_agreement_interpretive_kernel; the sibling readings instantiate different constraints with different victim sets - is the who-decides framing (this reading) or the what-it-mandates framing (siblings) the load-bearing locus of the contest?',
    'No in-framework resolution exists: the contest is constitutive. Resolution occurs only through precedent accumulation locking in a sibling, or through political override of the adjudicative channel as in the 2001 ministerial declaration on health.',
    'If a sibling reading captured the adjudicative apparatus, this constraint''s epsilon and victim structure would shift substantially - the who-decides arrangement would persist formally while deciding in the sibling''s terms, converting this story''s extraction profile into the sibling''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer-frame ambiguity: location of the kernel contest between interpretive authority and substantive readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_interp_auth_tr_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0, 0.18).
narrative_ontology:measurement(trips_interp_auth_tr_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 5, 0.21).
narrative_ontology:measurement(trips_interp_auth_tr_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 10, 0.24).
narrative_ontology:measurement(trips_interp_auth_tr_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 15, 0.28).
narrative_ontology:measurement(trips_interp_auth_tr_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 20, 0.33).
narrative_ontology:measurement(trips_interp_auth_tr_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 25, 0.46).
narrative_ontology:measurement(trips_interp_auth_tr_t30, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(trips_interp_auth_be_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(trips_interp_auth_be_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(trips_interp_auth_be_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(trips_interp_auth_be_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(trips_interp_auth_be_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(trips_interp_auth_be_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(trips_interp_auth_be_t30, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(trips_interp_auth_su_t0, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(trips_interp_auth_su_t5, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(trips_interp_auth_su_t10, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(trips_interp_auth_su_t15, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(trips_interp_auth_su_t20, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(trips_interp_auth_su_t25, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(trips_interp_auth_su_t30, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 30, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'TRIPS': the natural-language concept conflates an adjudicative meta-constraint (who settles textual meaning, this file) with two substantive readings of the text (what it mandates: strong_exclusivity_reading, public_health_flexibility_reading). Each family member has its own epsilon because the referents differ - this story's referent is the standing adjudicative arrangement; the siblings' referents are the substantive standards regimes. This story is upstream of both siblings in the causal sense that panel precedent determines which substantive reading becomes operative law, so its affects_constraints edges point at both. The siblings' files should carry reciprocal edges and document their own epsilon deltas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
