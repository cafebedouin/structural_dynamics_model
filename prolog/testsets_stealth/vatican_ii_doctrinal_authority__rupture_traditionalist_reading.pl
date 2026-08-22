% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__rupture_traditionalist_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__rupture_traditionalist_reading
 *   human_readable: Post-Conciliar Doctrinal Authority Settlement (Rupture-Traditionalist Reading)
 *   domain: ecclesiological/institutional-history/hermeneutic
 *
 * SUMMARY:
 *   Under the rupture-traditionalist construal, the operative constraint is
 *   the post-conciliar doctrinal settlement itself: a corpus of conciliar
 *   texts whose deliberately balanced formulations — products of compromise
 *   between competing drafting blocs — function as an authorization structure
 *   for implementation beyond anything the pre-conciliar magisterium taught.
 *   On this reading the ambiguities are load-bearing rather than accidental:
 *   they permit expansive implementation while preserving deniability, and
 *   the enforcement machinery alternately suppresses and tolerates the
 *   received liturgical and doctrinal inheritance as administrative
 *   convenience dictates. The settlement retains a genuine coordination
 *   function (a common reformed liturgy, standing ecumenical architecture,
 *   collegial governance), which is why this story claims tangled_rope rather
 *   than snare: coordination and extraction run through the same structure,
 *   and the extraction requires continuous enforcement to hold. KEY AGENTS
 *   (by structural relationship): - curial_magisterial_authorities: agenda
 *   setter (institutional/constrained) — administers and polices the
 *   settlement; alternately tightens and relaxes access to pre-conciliar
 *   forms - progressive_theologians: primary beneficiary (organized/mobile) —
 *   occupy the faculties, commissions, and platforms the settlement opened -
 *   heterodox_local_implementers: secondary beneficiary (powerful/mobile) —
 *   episcopal implementers exercising the latitude the texts' ambiguity
 *   licenses - ecumenical_partner_churches: incidental beneficiary
 *   (organized/mobile) — gain a counterpart relieved of pre-conciliar claims,
 *   at no cost to themselves - traditional_liturgy_practitioners: primary
 *   target (powerless/identity_locked) — bear the liturgical displacement and
 *   its restriction cycles - doctrinally_conservative_clergy: target
 *   (moderate/identity_locked) — bear career and formational costs under
 *   conformity-gated advancement - missionary_orders: target
 *   (organized/constrained) — bear the erosion of their founding
 *   evangelization mandate - dissenting_conciliar_fathers: excluded voice
 *   (organized/trapped) — reservations procedurally overridden at the council
 *   itself - comparative_ecclesiology_scholars: analytical observer
 *   (analytical/analytical) — see the full structure from outside the
 *   communion's disputes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "Post-Conciliar Doctrinal Authority Settlement (Rupture-Traditionalist Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, "ecclesiological/institutional-history/hermeneutic").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__rupture_traditionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'dc697d65-80ce-40ef-bc97-d4c268bb015f').
narrative_ontology:cs_kernel_codification('dc697d65-80ce-40ef-bc97-d4c268bb015f', fixed_text).
narrative_ontology:cs_authority_grounding('dc697d65-80ce-40ef-bc97-d4c268bb015f', lineage).
narrative_ontology:cs_interpretation_layer_present('dc697d65-80ce-40ef-bc97-d4c268bb015f').
narrative_ontology:cs_reading_relation('dc697d65-80ce-40ef-bc97-d4c268bb015f', vatican_ii_doctrinal_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('dc697d65-80ce-40ef-bc97-d4c268bb015f', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_axiom('dc697d65-80ce-40ef-bc97-d4c268bb015f', foundational, conciliar_texts_contain_doctrinal_defect).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_doctrinal_defect, holdable).
narrative_ontology:cs_axiom_grounding('dc697d65-80ce-40ef-bc97-d4c268bb015f', conciliar_texts_contain_doctrinal_defect, empirically_contingent).
narrative_ontology:cs_axiom('dc697d65-80ce-40ef-bc97-d4c268bb015f', foundational, traditional_liturgical_patrimony_irreversible).
narrative_ontology:cs_axiom_status(traditional_liturgical_patrimony_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('dc697d65-80ce-40ef-bc97-d4c268bb015f', traditional_liturgical_patrimony_irreversible, deontological).
narrative_ontology:cs_reference_frame('dc697d65-80ce-40ef-bc97-d4c268bb015f', preconciliar_integral_tradition).
narrative_ontology:cs_drift_state('dc697d65-80ce-40ef-bc97-d4c268bb015f', post_conciliar_implementation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('dc697d65-80ce-40ef-bc97-d4c268bb015f', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, heterodox_local_implementers).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_partner_churches).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_practitioners).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinally_conservative_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promulgates, implements, and polices the post-conciliar settlement through the Roman curia, the Congregation for Divine Worship, and doctrinal offices. Alternately relaxes and tightens access to the pre-conciliar liturgical forms (Ecclesia Dei 1988, Summorum Pontificum 2007, Traditionis Custodes 2021), disciplines institutes attached to the received rites, and arbitrates disputes over the documents' meaning. Collects governing flexibility from the settlement's capacious texts while bearing measurable membership and vocation decline across the period.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, curial_magisterial_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Staff theological faculties, liturgical commissions, catechetical offices, and editorial platforms that the settlement opened or expanded. The documents' balanced formulations license positions unavailable under the prior framework, and careers, publication fields, and advisory roles grew accordingly after 1965. Leaving the arrangement would mean abandoning the professional ecosystem it constitutes; there is nowhere else for this work to exist.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians, beneficiary,
    organized, biographical, mobile, global).

% Bishops, episcopal conferences, and diocesan bureaucracies who implement beyond the letter of the texts: communion disciplines for the remarried, intercommunion practices, vernacular and ritual adaptations, catechetical reframings. The ambiguity of the conciliar formulations supplies both the latitude and the deniability — appeal to 'the spirit of the Council' deflects responsibility back to the texts. They collect pastoral discretion while the costs of resulting doctrinal dispersion land elsewhere.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, heterodox_local_implementers, beneficiary,
    powerful, biographical, mobile, continental).

% Protestant communions and Orthodox churches engaged through the dialogue structures the settlement created. The arrangement retired claims and practices that had made Catholicism an existential competitor, giving these partners a counterpart that approaches them on terms of mutual enrichment rather than return. Their own doctrines, liturgies, and identities remain untouched by Catholic enforcement machinery; the accommodation flows one way.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, ecumenical_partner_churches, beneficiary,
    organized, generational, mobile, global).

% Laity and clergy whose devotional life is constituted by the 1962 missal and the received rites. The settlement removed their liturgy from ordinary availability in 1969-70, restored it conditionally in 1988 and 2007, and restricted it again in 2021. They fund traditional institutes out of pocket, absorb repeated restriction cycles, and cannot exit: leaving would mean abandoning the communion they hold to be the Church Christ founded, so the attachment itself binds them to the structure imposing the costs.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, traditional_liturgy_practitioners, payer,
    powerless, biographical, identity_locked, global).

% Priests and seminarians formed in pre-conciliar theology whose advancement runs through structures that treat their convictions as a liability. Objections to novel implementation are read as disloyalty rather than testimony; assignments, faculties, and preferment track conformity. Ordination binds them to the structure they dispute, and exit means forfeiting the priesthood itself — a cost few can pay, so they serve under terms they did not choose.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, doctrinally_conservative_clergy, payer,
    moderate, biographical, identity_locked, global).

% Congregations founded for the conversion of the unbaptized whose charter was reframed by the settlement's dialogue-first paradigm. Vocational pipelines and donor bases shifted toward social-service framing; explicit proselytism became suspect within their own governing documents. They continue operating missions worldwide but under an authorization structure that treats their founding purpose as an embarrassment, and dissolution of the congregation is the only exit from that mismatch.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, missionary_orders, payer,
    organized, generational, constrained, global).

% The bloc of council fathers (the Coetus Internationalis Patrum and allied petitioners) who contested draft wording on religious liberty and the liturgy constitution and sought clarifying amendments. Their proposals were deferred past voting deadlines and their reservations overridden by procedural momentum toward consensus. After promulgation their critique had no institutional channel, and the constituency they spoke for aged out of the conversation they were never conceded.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, dissenting_conciliar_fathers, excluded,
    organized, biographical, trapped, global).

% Historians and sociologists of religion who reconstruct drafting histories, vote counts, and implementation divergences across jurisdictions without allegiance to any faction's enforcement project. They can see the full structure at once — the texts, the interpretive apparatus, the restriction-and-relaxation cycles — and report on it from outside the communion's internal disputes.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, comparative_ecclesiology_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, progressive_theologians).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settlement coordinates the Church's encounter with modernity: it standardized a single reformed liturgy across the global communion, built standing ecumenical and interreligious dialogue architecture, reorganized governance around episcopal collegiality, and supplied a common pastoral vocabulary for a church that had previously governed by fixed rubric and hostile polemic.
% TRANSFER_FUNCTION: Moves doctrinal authority and liturgical patrimony from the inherited Tridentine framework to a newly constituted post-conciliar framework; moves discretion over doctrine, worship, and discipline from fixed rubrics to interpretive offices (bishops' conferences, liturgical commissions, theological faculties); and moves the transition's costs onto clergy and laity attached to the prior forms.
% ABSENT_VOICES: The dissenting conciliar fathers' reservations were procedurally overridden at the council itself and had no channel afterward; the lay faithful were never consulted on doctrinal reform and learned of changes as accomplished facts; traditionalist clergy after 1970 found their objections classified as disobedience rather than testimony, leaving the enforcing side to write the record of consent.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the communion would scramble: liturgical jurisdiction would fragment between surviving reformed books and the unreceived 1962 forms, the ecumenical architecture built on the new posture would lose its warrant, episcopal conferences would govern without their conciliar mandate, and the traditionalist constituency would find its claim vindicated amid institutional chaos — every named seat's arrangements depend on the settlement persisting.
% FOUNDING_PROBLEM: The pre-conciliar Church faced a perceived crisis of relevance and rigidity: liturgy experienced as distant from the faithful, ecumenical relations frozen in mutual anathema, missionary method criticized as coercive, and governance centralized to the point of brittleness. The settlement was built to engage the modern world without surrendering the deposit of faith.
% FOUNDING_PROBLEM_CORROBORATION: Secular religious sociology corroborates that the founding pressures were real — European attendance and vocation data show pre-conciliar decline, and diplomatic historians attest the Church's modern-era isolation. Orthodox and Protestant observers from outside the benefiting parties corroborate that pre-conciliar ecumenical hostility was genuine. No party outside the settlement's beneficiaries attests that the arrangement solves the founding problem rather than trading it for worse ones; on that question the corroborating sources divide along the same lines as the readings.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__rupture_traditionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at interval end) because the settlement's referent — the standing post-conciliar arrangement as this reading assesses it — displaced the received liturgy, diluted doctrinal clarity through compromise-drafted formulations, and rerouted the missionary mandate, with the costs landing on identifiable constituencies and the latitude accruing to identifiable ones. Suppression (0.72) reflects the end-state enforcement posture: Traditionis Custodes reversed two decades of liberalization and re-subordinated the received rites to episcopal permission. Theater ratio (0.48) sits just under half: invocation of 'the spirit of the Council' routinely exceeds the texts' letter, and anniversary and commemoration activity defends the settlement's authority rather than performing doctrine — but the underlying coordination (a functioning common liturgy, real dialogue channels) remains majority-functional. Accessibility collapse (0.60) is moderate-high: alternatives to the settlement persist but only conditionally and revocably, as the 1988-2007-2021 sequence demonstrates. Resistance (0.65) is substantial and organized: canonically irregular institutes, regularized traditional communities, the dubia correspondence, and filial-correction petitions all contest the settlement from inside. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream by directionality and scope in the engine's computation.
 *   
 *   The measurement series run on one shared time grid (1962, 1970, 1978, 1988, 2007, 2021) with every tracked metric authored at every point. The series are cyclical, not monotonic: imposition (1969-70) built extraction and suppression together; the concession phases (Ecclesia Dei 1988, Summorum Pontificum 2007) temporarily lowered both; reimposition (Traditionis Custodes 2021) raised both past their earlier peaks. The oscillation is plausibly part of the mechanism rather than noise — concessions absorb resistance while the settlement consolidates, and reimposition follows once the resisting constituency's demographic position has weakened, an intermittent-reinforcement dynamic. Base properties are measured at the end state (2021, reimposition phase), which is why the scalars sit at the series maxima.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same documents. From the beneficiary seats (progressive theologians, expansive implementers), the settlement is a coordination achievement they inhabit: capacious texts, professional expansion, pastoral discretion — a rope-like structure with negligible felt extraction. From the payer seats (traditional liturgy practitioners, conservative clergy, missionary orders), the same texts operate as an enforced transfer of patrimony and clarity, with exit priced at identity destruction — a heavily extractive structure held up by suppression. The agenda-setter seat straddles: it collects governing flexibility while bearing membership decline, and its own behavior (alternating relaxation and reimposition) reveals that it experiences the settlement as an instrument rather than a fate. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for progressive_theologians, heterodox_local_implementers, and ecumenical_partner_churches — the settlement subsidizes all three, and the first two hold arbitrage-grade mobility inside it. Victim declarations drive high directionality for traditional_liturgy_practitioners, doctrinally_conservative_clergy, and missionary_orders; the first two are additionally identity-locked, which places them nearer the full-target end than their formal exit options alone would suggest, since the binding is constitutive rather than contractual. The receipt surface names progressive_theologians as the seat the gains demonstrably accrue to: latitude, careers, and deniability concentrate there in ways the other beneficiary seats do not capture (the ecumenical partners receive accommodation without administering anything).
 *   
 *   One directionality override is authored: the institutional power atom is set to d=0.30. The derivation chain has no beneficiary/victim declaration for the curial agenda-setter seat and would fall back to a canonical default that misplaces a partially captured administrator — the curia collects real benefits from the settlement's flexibility (captured-administrator pull toward the beneficiary end) while bearing diffuse membership and credibility costs (pull back toward symmetry). The override encodes that mixed position; no other override is needed because the beneficiary/victim plus exit data already produce accurate directionalities for every remaining seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — engaging modernity without surrendering the deposit — is authored as contested rather than dead: the pressures the settlement answered were real and corroborated from outside the beneficiary set, so this is not a zombie mandate in the strict sense. But the settlement's justification has visibly drifted toward self-maintenance: enforcement activity increasingly defends the settlement's own authority (restricting the received rites, policing interpretive dissent) rather than performing the founding task, which is why theater_ratio trends upward across the interval and peaks alongside the 2021 reimposition. Fixing is prohibitively expensive for the only actor who could fix it: wholesale correction would require the papacy to repudiate an ecumenical council, at a credibility cost exceeding any benefit the fixer itself would collect — hence fixing_cost='prohibitive'. The tangled_rope claim prevents two symmetrical mislabels: flattening the settlement into pure extraction would erase the genuine coordination (common liturgy, dialogue architecture) that even this reading concedes; excusing the extraction as necessary adaptation cost would erase the identifiable victims and the enforcement dependence that make the asymmetry structural. The R5 mismatch consumer should read status='contested' x verdict='world_rearranges' as no dead-mandate flag but as a drift-watch condition: if the founding problem's liveness collapses (status moving to dead) while the enforcement machinery keeps ratcheting, the settlement crosses from contested hybrid toward inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (rupture_traditionalist_reading) of the vatican_ii_doctrinal_authority kernel; which structural facts would change if a sibling reading were adopted instead?',
    'Not resolvable by data within this story: generate and classify the sibling stories (continuity_reading, rupture_progressive_reading, composite_overdetermination_reading) and compare computed classifications across the family.',
    'Under continuity_reading the victim set empties and extraction collapses toward coordination cost; under rupture_progressive_reading the same high epsilon is positively valued and the victim set shifts to casualties of pre-conciliar rigidity; under composite_overdetermination_reading the unified settlement dissolves into separately classified liturgical, ecumenical, ecclesiological, and political shifts with distinct epsilons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: reading-indexed classification of a shared kernel; sibling adoption changes victim sets, epsilon polarity, and type.').

omega_variable(
    ambiguity_intentionality,
    'Are the conciliar texts'' ambiguities compromise-induced defects (this reading''s claim) or providentially capacious formulations that later interpreters abused?',
    'Drafting-archive study: relatio summaries, rejected modi, roll-call vote tallies, and minority petitions compared against post-promulgation implementation trajectories.',
    'If the ambiguities are authored defects, epsilon attaches to the texts themselves and the settlement is extractive by design; if they are capaciousness, epsilon attaches to the interpreters and the settlement''s own extraction drops toward the coordination-cost floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentionality, empirical, 'Whether textual ambiguity is an authored property of the settlement or an artifact of downstream interpretation.').

omega_variable(
    implementation_causality_direction,
    'Does heterodox implementation flow from the texts'' ambiguity, or from secularizing currents the texts attempted to channel?',
    'Cross-jurisdiction comparison: dioceses and conferences that implemented strictly versus expansively under identical texts, tracking doctrinal and liturgical outcome divergence across decades.',
    'Outcome divergence under a fixed text supports attributing high epsilon to the settlement itself; convergence regardless of implementation style shifts causal weight to ambient culture and lowers the settlement''s attributed extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_causality_direction, empirical, 'Causal arrow between textual ambiguity and heterodox implementation.').

omega_variable(
    suppression_structural_vs_internalized,
    'For traditional liturgy practitioners (identity_locked), is the suppression keeping them in place structural (no alternative communion they recognize as valid) or internalized (identity fused with the rite and the Church it anchors)?',
    'Post-exit trajectory of those who do leave (toward sedevacantist positions or Eastern communion): if rite-centered attachment persists independently of Roman structure, the internalized share is high; if attachment tracks the recognized-validity question, it is structural.',
    'If largely internalized, effective suppression exceeds the structural measure and survives enforcement relaxation; if structural, enforcement changes translate directly into realized exit and the 2007-2021 liberalization window should have shown measurable outflow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism split for identity-locked traditionalist attachment.').

omega_variable(
    rupture_valuation_underdetermination,
    'The descriptive claim that rupture occurred is shared with rupture_progressive_reading; only the valuation differs — is the negative valuation a structural fact or a preference the classification machinery cannot settle?',
    'Not resolvable by evidence: it turns on whether fidelity to received forms outranks adaptive capacity in the evaluator''s value ordering.',
    'This story''s classification is stable either way, but cross-reading comparison will show mirrored epsilon with opposed polarity; consumers must not read the polarity difference between this reading and rupture_progressive_reading as an empirical finding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rupture_valuation_underdetermination, preference, 'Preference-indexed valuation of a descriptively shared rupture claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, 1962, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1978, 0.42).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 1988, 0.4).
narrative_ontology:measurement(vati_tr_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2007, 0.36).
narrative_ontology:measurement(vati_tr_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, theater_ratio, 2021, 0.48).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1962, 0.3).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1970, 0.62).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1978, 0.7).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 1988, 0.66).
narrative_ontology:measurement(vati_be_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2007, 0.58).
narrative_ontology:measurement(vati_be_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, base_extractiveness, 2021, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1962, 0.2).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1978, 0.65).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 1988, 0.55).
narrative_ontology:measurement(vati_su_t2007, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2007, 0.4).
narrative_ontology:measurement(vati_su_t2021, vatican_ii_doctrinal_authority__rupture_traditionalist_reading, suppression_requirement, 2021, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what Vatican II was' covers four structurally distinct claims and decomposes into four stories linked by affects_constraints. Epsilon differs across the family: continuity_reading authors low extraction (organic development, no victim set); rupture_progressive_reading and this story author high extraction over the same descriptive rupture with opposed polarity (positively valued reform latitude versus negatively valued patrimony transfer); composite_overdetermination_reading rejects the unified object entirely and splits it into separately classified shifts. Upstream/downstream structure: continuity_reading is the magisterium's official frame and supplies the legitimacy conditions both rupture readings react against; this story and rupture_progressive_reading are downstream rivals that share the rupture premise and differ only in valuation. Each file carries its own epsilon, beneficiaries, victims, and claimed type; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__rupture_traditionalist_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
