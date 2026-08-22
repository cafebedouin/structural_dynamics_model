% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Conciliar Settlement as Unified Authoritative Package (Composite Overdetermination Reading)
 *   domain: ecclesiological/institutional-history/hermeneutic
 *
 * SUMMARY:
 *   Under the composite overdetermination reading, Vatican II is not one
 *   shift but a convergence of structurally distinct changes - liturgical,
 *   ecumenical, ecclesiological, political - ratified as a single package
 *   whose ambiguities are load-bearing: they are what allowed opposed camps
 *   to sign the same documents and what now concentrate the authority to say
 *   what the documents mean. This story authors THAT settlement layer: the
 *   standing arrangement under contest is the conciliar corpus operating as a
 *   unified authoritative package governed by an interpretive center, and
 *   epsilon is assessed for that arrangement by this reading's own lights -
 *   not for the reading's preferred alternative (full component-by-component
 *   settlement), which would drive epsilon toward zero by construction. The
 *   claim/metric gap is deliberate: the settlement is CLAIMED here as
 *   tangled_rope (genuine coordination holding a global institution together
 *   through change, carrying asymmetric extraction through the same
 *   structure), while the metrics are authored descriptively from the
 *   enforcement record; the engine computes per-seat classifications and any
 *   divergence from the claim is the datum. KEY AGENTS (by structural
 *   relationship): roman_curia_papacy - agenda-setting interpreter
 *   (institutional/arbitrage) - collects the arbitration advantage;
 *   traditionalist_clergy_laity - primary bearing constituency
 *   (organized/identity_locked) - pays in patrimony, jurisdiction, liturgical
 *   access; progressive_theologians - dual-positioned (moderate/constrained)
 *   - collects research space, pays disciplinary costs;
 *   ordinary_parish_faithful - diffuse bearing constituency
 *   (moderate/constrained); national_bishops_conferences - dual-positioned
 *   (institutional/trapped); ecumenical_dialogue_partners - subsidized
 *   counterpart (institutional/mobile); professional_conciliar_hermeneuts -
 *   subsidized interpreter (moderate/mobile); independent_church_historians -
 *   analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.67).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Conciliar Settlement as Unified Authoritative Package (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiological/institutional-history/hermeneutic").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'a0827aba-2233-427f-933a-fabc6aca0e6a').
narrative_ontology:cs_kernel_codification('a0827aba-2233-427f-933a-fabc6aca0e6a', fixed_text).
narrative_ontology:cs_authority_grounding('a0827aba-2233-427f-933a-fabc6aca0e6a', lineage).
narrative_ontology:cs_interpretation_layer_present('a0827aba-2233-427f-933a-fabc6aca0e6a').
narrative_ontology:cs_reading_relation('a0827aba-2233-427f-933a-fabc6aca0e6a', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a0827aba-2233-427f-933a-fabc6aca0e6a', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('a0827aba-2233-427f-933a-fabc6aca0e6a', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_axiom('a0827aba-2233-427f-933a-fabc6aca0e6a', foundational, conciliar_plurality_irreducible).
narrative_ontology:cs_axiom_status(conciliar_plurality_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('a0827aba-2233-427f-933a-fabc6aca0e6a', conciliar_plurality_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('a0827aba-2233-427f-933a-fabc6aca0e6a', foundational, ambiguities_load_bearing).
narrative_ontology:cs_axiom_status(ambiguities_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('a0827aba-2233-427f-933a-fabc6aca0e6a', ambiguities_load_bearing, empirically_contingent).
narrative_ontology:cs_reference_frame('a0827aba-2233-427f-933a-fabc6aca0e6a', heterogeneous_packaged_settlement).
narrative_ontology:cs_drift_state('a0827aba-2233-427f-933a-fabc6aca0e6a', post_traditionis_custodes_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('a0827aba-2233-427f-933a-fabc6aca0e6a', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia_papacy).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, professional_conciliar_hermeneuts).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_dialogue_partners).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_clergy_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ordinary_parish_faithful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates the conciliar corpus, issues authoritative interpretations through congregations and papal documents, and decides which readings of ambiguous passages are licit, disciplining deviant ones. Because no text settles the contested questions outright, the office that answers what the Council requires holds discretionary power over every faction. One pope widened access to the older liturgy and a successor narrowed it again, showing the office can reframe its own arrangements. Exit is meaningless at this seat: the office constitutes the interpretive center.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia_papacy, agenda_setter,
    institutional, generational, arbitrage, global).

% Communities formed around the pre-conciliar liturgy and doctrinal inheritance. They experience the conciliar changes as loss - a replaced rite, altered relations with other Christians, revised teaching on religious liberty - while the official account tells them nothing essential changed. Their preferred forms of worship are available only by permission and were restricted again in 2021. Some organized bodies operate outside ordinary jurisdiction under canonical penalties. Leaving the fold entirely forfeits the sacramental life their identity centers on; remaining means accepting interpretations they regard as distortions.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_clergy_laity, payer,
    organized, generational, identity_locked, global).

% Scholars and pastoral figures who received new working space from the conciliar openings: religious liberty, ecumenism, collegiality, tolerated historical-critical method. The unsettled meanings supply their research programs and influence. The same center that opened the space disciplines them when conclusions outrun it - censures, investigations, removal from teaching posts. Careers depend on Catholic institutions, so departure carries professional ruin; staying means calibrating how far development can go before the center responds.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_theologians, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_theologians, beneficiary).

% Lay Catholics who absorbed the practical changes: a new liturgy learned mid-life, catechetical shifts, a changed devotional landscape. They bear implementation churn without a seat in any drafting or interpretation body; disputes between factions arrive at parish level as repeated changes in practice. Exit runs from quiet disengagement to joining other denominations, both costly where parish life anchors family and community ties.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ordinary_parish_faithful, payer,
    moderate, biographical, constrained, global).

% Regional episcopal bodies that gained formal standing from the conciliar teaching on collegiality: liturgical translation oversight, conference-level governance, recognized voice in universal deliberation. They also find decisions overruled from Rome when the center prefers a different line, as translation and liturgical supervision disputes have shown. They cannot leave the episcopal college; their standing depends on the very arrangement that sometimes overrides them.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences, beneficiary,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences, payer).

% Other churches and ecclesial communities - Orthodox, Protestant, Anglican - that gained recognition as dialogue partners rather than adversaries. Conciliar decrees acknowledged their baptism and ecclesial reality in qualified terms and committed Catholics to structured dialogue. They participate as invited counterparts; the terms of recognition remain defined by the Catholic side's documents, and they hold no lever over how unsettled questions are eventually answered.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_dialogue_partners, beneficiary,
    institutional, generational, mobile, global).

% Historians and theologians whose livelihoods and reputations rest on interpreting the conciliar texts: editing the acts, reconstructing drafting histories, advising on what the Fathers intended. The unsettled meanings are inexhaustible working material; a definitive settlement of every question would retire much of the field. They populate pontifical commissions and faculties; moving to adjacent fields is feasible but costly to standing.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, professional_conciliar_hermeneuts, beneficiary,
    moderate, biographical, mobile, continental).

% Scholars outside confessional employment who study the council from archives, comparing internal accounts with external documentation. They hold no stake in which reading prevails and can state what partisans cannot.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, independent_church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, roman_curia_papacy).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative reference corpus that let a global institution absorb epochal modernization - liturgical change, religious liberty, ecumenical recognition, collegiality - without formal division, by giving every faction the same texts to appeal to while leaving their application open.
% TRANSFER_FUNCTION: Moves interpretive authority and disciplinary discretion toward the Roman center, which alone settles what the ambiguous texts require; moves compliance costs outward to the factions whose readings lose - liturgical patrimony and jurisdiction from traditionalists, speculative freedom from progressives, stability and formation continuity from ordinary faithful.
% ABSENT_VOICES: The conciliar minority - the bloc of council fathers who fought the prepared schemas amendment by amendment - signed the final texts under obedience and left no institutional seat in the implementation bodies that followed; their reservations survive mainly in diaries and private papers. Traditionalist laity had no representation in liturgical implementation committees. Irregular traditionalist jurisdictions are addressed through canonical penalty rather than consultation. Ordinary faithful were never consulted on any of it; the dissenting seats were outside the room when the package's meaning was fixed.
% DISAPPEARANCE_RATIONALE: Every post-conciliar arrangement presupposes the package: the current liturgy's legality, the ecumenical dialogues' charter, the curia's interpretive role, the theological faculties' agendas. Overnight removal would reopen every question the package closed - liturgical form, religious-liberty teaching, ecumenical recognition, collegiality - and the factions currently held inside one institution by the shared reference corpus would organize around rival settlements.
% FOUNDING_PROBLEM: How to enable a global, legally anchored church to engage twentieth-century modernity - constitutional religious liberty, vernacular culture, divided Christendom, historical consciousness - while preserving doctrinal authority and institutional unity; concretely, how to draft texts that a supermajority spanning opposed theological camps could ratify as one reform.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of the council (the Bologna school's archival editions; O'Malley's account of the rhetorical shift) corroborate both the reality of the engagement problem and the packaging mechanics from outside the benefiting parties. Orthodox and Protestant dialogue partners corroborate that the engagement problem was real. The conciliar minority's private diaries corroborate that ratification required deliberate constructive ambiguity. No party outside the disputing camps attests that the resulting hermeneutical conflict is resolved.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (end-state) because the settlement's costs are real and unevenly placed - restricted liturgical access, censured scholarship, implementation churn - while its benefits are genuine and shared. Suppression is authored at 0.67 as a RAW STRUCTURAL PROPERTY, unscaled by power or scope: the settlement's persistence depends on active enforcement of interpretive boundaries (canonical penalties, permission regimes for the older liturgy, doctrinal assessments), not on voluntary assent; only extractiveness gets scaled by directionality and scope in the engine's computation. Theater_ratio 0.45 reflects a growing share of performative unity - anniversary pageantry, consensus rhetoric, synodal process - alongside real but shrinking substantive mediation. Accessibility_collapse 0.45: alternatives persist (other communions, irregular jurisdictions, disengagement) but each is costly, so the settlement does not face mountain-grade inevitability nor leave cheap exits. Resistance 0.62: sustained, organized, multi-decade resistance from both flanks. The temporal series run on ONE SHARED GRID (1965, 1970, 1978, 1988, 2007, 2021, 2025) with every tracked metric authored at every point; the 2007 dip in all three series tracks the deliberate accommodation that widened access to the older liturgy, and the 2021 spike tracks its reversal - the series are one account of enforcement-first-softened-then-hardened, not independent curves. The trajectory is not cyclical; it is a ratchet with one counter-move.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the curial seat the settlement reads as workable governance: ambiguity is flexibility, enforcement is stewardship, and the alternatives (division or stagnation) justify the costs imposed. From the traditionalist seat the same structure reads as enforced dispossession: an inheritance ruled negotiable by texts never consented to, interpreted by an office that profits from the interpretation. From the progressive seat it reads as a ratchet: openings granted and then clawed back when development outran tolerance. Identity-lock sharpens the traditionalist divergence - the community's self-concept as custodian of an unbroken tradition fuses with its position, so even the 2007 permission regime failed to convert it into a stakeholder in the settlement; breaking that identity frame would change its exit profile from locked to merely costly. Coalition potential among the bearing seats is structurally weak: traditionalists and progressives want opposite settlements, so the diffuse lay majority absorbs the churn alone.
 *
 * DIRECTIONALITY LOGIC:
 *   The curia-papacy seat sits near the beneficiary pole: it collects the arbitration advantage and bears almost none of the settlement's costs. Hermeneuts and ecumenical partners are subsidized seats - the arrangement generates their working material and standing - so their derived directionality lands low. Bishops' conferences derive mildly beneficiary with a payer overlay captured by the secondary role. Traditionalists derive near-full-target: they pay in patrimony, jurisdiction, and worship access, and identity_lock pushes them toward the trapped end of exit, which the engine weights toward full-target. Progressive theologians derive mid-to-high: discipline costs paid, research space collected. Ordinary faithful derive mid: diffuse payment in stability and formation, weakly organized. The settlement's GLOBAL spatial scope makes verification of compliance harder everywhere, which the engine converts into amplified effective extraction on the target seats; the authored data supplies the structure and the engine owns the arithmetic. No directionality overrides are used: the beneficiary/victim declarations plus exit profiles already differentiate the seats, and the override surface is keyed by power atom, which would smear corrections across unrelated institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem splits in two. The ratification problem - getting a supermajority of opposed camps to sign one reform - is DEAD: the package passed in 1965 and cannot be un-passed. The engagement-with-modernity problem remains LIVE, and the packaging acquired a second life after its original function ended: interpretive arbitration. The R5 mismatch signature (status contested, verdict world_rearranges, ratification function deceased) flags the zombie component for investigation rather than letting either cover story stand. The classification prevents two symmetrical mislabelings: reading the settlement as pure coordination (the official continuity account) erases who pays for the ambiguity and who collects the arbitration advantage; reading it as pure rupture-and-usurpation (the traditionalist account) erases the demonstrated coordination - the same corpus held a global institution through changes that have split far smaller bodies. Holding both facts on the table is what the tangled_rope claim does, and the temporal record shows the extraction component accumulating whenever enforcement substitutes for settlement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_individuation,
    'Is the composite-overdetermination account the correct individuation of the conciliar settlement, or does one of the unitary sibling readings (continuity, rupture-progressive, rupture-traditionalist) describe the same object better?',
    'Archival drafting and voting history: if the components show independent drafting coalitions, distinct vote margins, and separately negotiated compromises, the composite account is confirmed; a uniform pattern across components would support a unitary reading.',
    'Sibling readings instantiate different constraints with different epsilon and different victim sets: continuity collapses extraction toward coordination cost; rupture-traditionalist raises epsilon and relocates the responsible seat from administrator to author. This story''s classification is conditional on the individuation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_individuation, conceptual, 'Whether the composite reading or a unitary sibling correctly individuates the conciliar settlement.').

omega_variable(
    component_epsilon_independence,
    'Do the four components (liturgical, ecumenical, ecclesiological, religious-liberty) carry genuinely independent extractiveness such that this story''s aggregate epsilon conceals divergent component-level values?',
    'Author component-level stories and compare: liturgical reform is expected to measure high extraction from traditionalist constituencies; the religious-liberty decree is expected to measure near-subsidy for religious minorities. Wide divergence across component epsilon confirms the composite diagnosis.',
    'Wide divergence shifts analytic weight to the component stories and marks this story''s epsilon as a weighted aggregate; narrow divergence would suggest the packaging itself, not the components, is the operative structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_epsilon_independence, empirical, 'Whether component-level extractiveness is independent enough to require separate stories under the epsilon-invariance principle.').

omega_variable(
    ambiguity_intentionality,
    'Were the load-bearing ambiguities a deliberate drafting strategy or emergent compromise artifacts?',
    'Commission diaries, relatio explanations, and the modi process: evidence that drafters rejected clarifying amendments to preserve multi-factional signability indicates design; ambiguities appearing only where coalitions deadlocked indicate emergence.',
    'Designed ambiguity makes the center''s interpretive advantage a designed feature of the arrangement, sharpening the extraction reading; emergent ambiguity makes it opportunistic capture of an accident, softening the reading toward hybrid coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentionality, empirical, 'Deliberate versus emergent origin of the settlement''s productive ambiguities.').

omega_variable(
    hermeneutic_resolvability,
    'Can any authoritative interpretation settle the ambiguities without dissolving the package, or is unresolvability structural to the texts'' construction?',
    'Track the fate of settlement attempts: the 2007 widening and 2021 narrowing of access to the older liturgy; if each attempted settlement generates new enforcement burdens without reducing factional conflict, unresolvability is structural.',
    'Resolvable ambiguities would let the arrangement mature toward a lower-extraction steady state; structural unresolvability predicts continued drift toward enforcement substituted for settlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutic_resolvability, conceptual, 'Whether the settlement''s ambiguities admit authoritative resolution at all.').

omega_variable(
    enforcement_ratchet_trajectory,
    'Is the post-2021 suppression intensification a temporary centralizing correction or the opening of a durable ratchet?',
    'Enforcement statistics and policy reversals over the following decade: reversal or negotiated accommodation indicates correction; broadening restrictions to adjacent constituencies indicates ratchet.',
    'Correction supports tangled-rope stability at the current profile; ratchet supports drift toward snare-flavored operation at the traditionalist seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_trajectory, empirical, 'Trajectory of the post-2021 enforcement intensification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1970, 0.27).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1978, 0.31).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1988, 0.38).
narrative_ontology:measurement(vati_tr_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2007, 0.33).
narrative_ontology:measurement(vati_tr_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2021, 0.43).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.46).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1970, 0.54).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1978, 0.58).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1988, 0.61).
narrative_ontology:measurement(vati_be_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2007, 0.56).
narrative_ontology:measurement(vati_be_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2021, 0.66).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.34).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1970, 0.47).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1978, 0.53).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1988, 0.61).
narrative_ontology:measurement(vati_su_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(vati_su_t2021, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2021, 0.69).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_liturgical_reform_component).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_religious_liberty_component).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Vatican II' covers structurally distinct claims. This story authors the packaging/settlement layer - the unified corpus and its interpretive economy. Component layers (liturgical reform, the religious-liberty decree, the ecumenical decree, the collegiality settlement) carry independent extractiveness profiles and are authored as separate stories linked here; the continuity and rupture sibling readings are likewise separate constraints, not alternative observables of this one. Upstream/downstream: the settlement layer influences the component stories (its enforcement posture sets their operating conditions), and the sibling readings compete with this one at the individuation level documented in the kernel_reading_individuation omega.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
