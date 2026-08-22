% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Orthodox Restitution Reading of the Hagia Sophia Legitimacy Kernel
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the hagia_sophia_substrate kernel:
 *   the orthodox_restitution_reading, which holds that the site's legitimacy
 *   flows from its 537 founding consecration as the Great Church of
 *   Constantinople and that rightful disposition is therefore Orthodox
 *   ecclesial control or, failing that, neutral custodianship honoring the
 *   Byzantine origin. The claim is real and institutionally maintained -
 *   anniversary statements, liturgical commemoration of the city's fall,
 *   diplomatic protest notes, diaspora commemorative infrastructure - but it
 *   commands no enforcement machinery, transfers nothing, and compels
 *   nothing; its operation is almost entirely symbolic and discursive.
 *   Epsilon's referent here is the claim's actual operation in the standing
 *   discourse (what it imposes on its targets and what maintaining it costs
 *   and yields), NOT the restored-Orthodox arrangement the reading endorses -
 *   that alternative is never rated. The sibling readings
 *   (islamic_sovereignty_reading, universal_heritage_reading) are separate
 *   constraints in separate files with their own epsilon values, beneficiary
 *   sets, and victim sets; this file links to them through the network
 *   surface. KEY AGENTS (by structural relationship):
 *
 * KEY AGENTS:
 *   - - greek_state: Agenda-setting maintainer (institutional/constrained) - administers the claim's official diplomatic expression, collects narrative leverage, bears bilateral friction with a NATO ally
 *   - - ecumenical_patriarchate: Ecclesial custodian-beneficiary with payer exposure (moderate/identity_locked) - keeps the claim's doctrinal content alive while declining material advocacy under Turkish legal pressure
 *   - - eastern_orthodox_diaspora: Primary symbolic beneficiary (organized/identity_locked) - organizes communal belonging around the claim's loss-and-fidelity narrative
 *   - - turkish_sovereignty_interests: Primary target (institutional/trapped) - bears the standing external claim on national territory and the conquest settlement's legitimacy
 *   - - istanbul_muslim_worshipping_community: Secondary target (organized/constrained) - bears delegitimation pressure on worship continuity at the site
 *   - - unesco_heritage_regime: Analytical observer (institutional/analytical) - documents and conserves without adjudicating which historical layer confers title
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.33).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.3).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.33).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, piton).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Orthodox Restitution Reading of the Hagia Sophia Legitimacy Kernel").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/sovereignty/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, '20cf0483-9d67-47fa-8aac-dff31997e1bd').
narrative_ontology:cs_kernel_codification('20cf0483-9d67-47fa-8aac-dff31997e1bd', distributed).
narrative_ontology:cs_authority_grounding('20cf0483-9d67-47fa-8aac-dff31997e1bd', lineage).
narrative_ontology:cs_interpretation_layer_present('20cf0483-9d67-47fa-8aac-dff31997e1bd').
narrative_ontology:cs_reading_relation('20cf0483-9d67-47fa-8aac-dff31997e1bd', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('20cf0483-9d67-47fa-8aac-dff31997e1bd', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('20cf0483-9d67-47fa-8aac-dff31997e1bd', foundational, founding_consecration_indelible).
narrative_ontology:cs_axiom_status(founding_consecration_indelible, holdable).
narrative_ontology:cs_axiom_grounding('20cf0483-9d67-47fa-8aac-dff31997e1bd', founding_consecration_indelible, theological).
narrative_ontology:cs_axiom('20cf0483-9d67-47fa-8aac-dff31997e1bd', secondary, conquest_does_not_extinguish_ecclesial_title).
narrative_ontology:cs_axiom_status(conquest_does_not_extinguish_ecclesial_title, holdable).
narrative_ontology:cs_axiom_grounding('20cf0483-9d67-47fa-8aac-dff31997e1bd', conquest_does_not_extinguish_ecclesial_title, deontological).
narrative_ontology:cs_reference_frame('20cf0483-9d67-47fa-8aac-dff31997e1bd', byzantine_consecrated_cathedral_order).
narrative_ontology:cs_drift_state('20cf0483-9d67-47fa-8aac-dff31997e1bd', contemporary_post_2020_reconversion, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('20cf0483-9d67-47fa-8aac-dff31997e1bd', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty_interests).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, istanbul_muslim_worshipping_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, sacramental_indelibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the claim in official diplomatic discourse: protest notes over the site's status, annual statements on the anniversary of the city's fall, curriculum treatment of 1453. Gains narrative standing at home and a standing card in bilateral dealings; pays in periodic friction with a NATO ally and in expectations it cannot cash. Dropping the claim would cost domestic political capital across the spectrum; escalating it invites crises it does not want.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary).

% Custodian of the ecclesial memory the claim draws on: marks the fall of the city liturgically, laments the site's status, and has declined for decades to advance any material demand for control, operating under legal constraints inside Turkey - property seizures, the closed seminary - that make open advocacy dangerous. Its worldwide standing rises with the claim's circulation; its local security falls when tensions spike. It cannot abandon the see or the commemorative stance without dissolving what it is.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, beneficiary,
    moderate, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, ecumenical_patriarchate, payer).

% Communities in Greece, North America, Australia, and elsewhere organize belonging around the Great Church narrative: pilgrimages, commemoration societies, parish education, fundraising for the Patriarchate. The claim anchors a shared story of loss and fidelity. Few members expect the site's control to change; participation is sustained by memory rather than expectation, and stepping outside the commemorative framework carries real social cost in the communities that maintain it.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    organized, generational, identity_locked, global).

% Holds and administers the site under the 1934 decree and the 2020 reconversion decision. Treats the external restitution claim as a standing challenge to national territory and to the legality of the conquest-era settlement, responding with diplomatic rebuttal and periodic domestic mobilization. Conceding any portion of the claim carries prohibitive domestic cost; no action available to it discharges the claim or stops its recurrence.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty_interests, payer,
    institutional, generational, trapped, national).

% Prays in the congregations the site hosts and carries the endowment tradition of continuous Islamic use since 1453. The external claim asserts that their worship occupies a place that rightfully lies elsewhere; they bear the delegitimation pressure and the uncertainty each diplomatic episode injects. Their day-to-day access depends on Turkish state policy rather than on anything the claim's holders could grant or withhold.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, istanbul_muslim_worshipping_community, payer,
    organized, biographical, constrained, national).

% Lists the property as World Heritage and engages the site through conservation and universal-value framing. Periodically urges dialogue among the claim's holders, holds no position on ecclesial restitution, and treats the dispute as a site-management question. Its documentation records the building's layered history without adjudicating which layer confers title.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, unesco_heritage_regime, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__orthodox_restitution_reading, diffuse).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__orthodox_restitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Eastern Orthodox diaspora identity and Greek national memory around a shared sacred-center narrative: a common commemorative calendar, institutional positioning for the Patriarchate and diaspora organizations, and a standing account of historical injustice that organizes communal belonging across borders.
% TRANSFER_FUNCTION: Transfers symbolic standing and narrative leverage rather than material goods: the claim taxes Turkish sovereignty's legitimacy narrative and Islamic worship's security at the site, and converts that friction into identity capital, commemorative cohesion, and diplomatic signaling for Greek and Orthodox actors. Nothing else moves.
% ABSENT_VOICES: Muslim worshippers at the site and Turkish heritage authorities are absent from the claim's internal discourse - the reading deliberates the site's rightful future without the people who pray there now. Also largely absent: Istanbul's remaining Greek community, whose lived priorities of safety and coexistence differ from diaspora-maximalist commemoration, and whose shrinking numbers make it the claim's least consulted constituency.
% DISAPPEARANCE_RATIONALE: If the claim vanished overnight, the diaspora commemorative calendar loses its central anchor, Greek diplomatic repertoire loses a standing card, Patriarchal witness rhetoric loses its sharpest edge, and Turkish rebuttal routines lose their foil - identities and institutional practices are arranged around the claim's existence. The rearrangement would be almost entirely at the symbolic-identity level; material arrangements at the site would continue unchanged.
% FOUNDING_PROBLEM: Undo the religious consequence of the 1453 conquest: restore the Great Church to Orthodox ecclesial control, or failing that, secure neutral status for the building that honors its founding consecration.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated as dead from outside the beneficiary set: the Turkish state's sovereignty position, unchallenged in any international forum; the absence of any Greek government position advancing restitution as a negotiable objective since the 1923 settlement; UNESCO's administration of the property under an unrelated framing; and the Ecumenical Patriarchate's own decades-long refusal to press a material claim despite occasions (including the 2020 reconversion) when it could have. No party outside the claim's adherents attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.33, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low-moderate (0.33): the claim taxes its targets' legitimacy narratives and injects episodic diplomatic friction, but transfers nothing material and compels nothing - the manifest's 'symbolically generative, materially dormant' profile. Suppression (0.30) is raw and unscaled: no state enforcement exists anywhere in the structure; what remains is discursive policing inside adherent communities (renunciation framed as betrayal) plus reputational enforcement that intensified rhetorically after the 2020 reconversion. Theater dominates (0.72): anniversary statements, commemorative liturgies, and museum-era nostalgia constitute nearly the whole contemporary operation, while the functional content - actual pursuit of a change in the site's disposition - is near nil. Accessibility collapse is low (0.30): alternatives are robustly alive, with two sibling readings and simple pragmatic acceptance persisting inside and outside the adherent community. Resistance is substantial (0.62): outright rejection by the target state, a competing universal-heritage framing, and abstention even from the claim's natural ecclesial champion. The temporal series runs on one shared eight-point grid (interval years since the 1923 settlement). The suppression_requirement series is authored because enforcement capacity is precisely what changed: state and military backing collapsed after 1923, ticked up with mid-century commemorative mobilization and the Cyprus-era confrontation, decayed through the rapprochement years, and partially reconstituted in purely rhetorical-reputational form after 2020. Theater rises monotonically across the whole interval - classic proxy displacement, memory-performance replacing the disposition-change function. The small post-2013 upticks in extraction and suppression are an exogenous shock (the 2020 reconversion), not a cyclical oscillation; no intermittent-reinforcement dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute sharply different types from identical structural data. From the turkish_sovereignty_interests seat the claim is an uninvited standing lien on territory that no available action can discharge - imposed, recurring, cost-bearing. From the istanbul_muslim_worshipping_community seat it is delegitimation of prayer itself, an assertion that their worship occupies a place that rightfully lies elsewhere. From the eastern_orthodox_diaspora seat the same structure is memory-keeping: costless at the margin, obligatory, constitutive. From the greek_state seat it is a cheap diplomatic asset with tail risk. The unesco observer seat sees an inertial remnant carried because retiring it costs more than holding it. The engine computes this divergence from the role, power, and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the diaspora and state seats toward the beneficiary end of d; victim declarations drive the sovereignty and worship-continuity seats toward the target end, amplified by trapped and constrained exits respectively. Two overrides are declared where the automatic derivation would misread the relationship. First, greek_state: the beneficiaries[] listing alone would derive a near-pure-beneficiary d, but the state demonstrably absorbs costs (bilateral friction, alliance management, escalation risk), so d is overridden to 0.25 - still beneficiary-side, honestly cost-bearing. Second, ecumenical_patriarchate: it collects worldwide symbolic standing from the claim's circulation while bearing Turkish legal pressure that spikes with the claim's salience, and its identity_locked exit amplifies that exposure; d is overridden to 0.35. Suppression stays a raw structural property throughout; only extraction is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - undoing the religious consequence of the 1453 conquest - died as a practical program with the 1923 settlement and the population exchange, which removed both the material pathway and the local constituency able to pursue it. The arrangement persisted anyway, transmuting into commemorative infrastructure. The classification guards against both misreadings. Reading the claim as a live hybrid coordination-extraction structure would overstate it: nothing binds anyone, no enforcement exists, and no seat captures the gains (receipt is affirmatively diffuse - the diaspora, state, and patriarchate all benefit-from the claim, but nothing is transferred from its targets to any of them). Reading it as pure empty ritual would understate it: the identity-coordination function is genuine, and the symbolic imposition on the target seats is real and recurring. The inertial-remnant designation holds both facts: function mostly atrophied, persistence by inertia and performance, retirement blocked by a cost asymmetry - the agenda-setter could drop the claim, but the domestic narrative and diaspora-trust cost of doing so exceeds any benefit, while no seat is hurt enough by carrying it to force retirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_multiplicity,
    'This constraint is one reading of the hagia_sophia_substrate kernel (reading: orthodox_restitution_reading); what would the sibling readings structurally change, and where exactly is the disagreement located?',
    'No in-framework resolution exists: the disagreement is located in the title-grounding premise itself - founding consecration versus conquest endowment versus transcendent heritage - and is settled, if at all, by political and theological developments outside any single reading''s control.',
    'Adopting a sibling reading inverts or dissolves this constraint''s beneficiary and victim sets: under the islamic sovereignty reading the Orthodox claim becomes the external imposition; under the universal heritage reading all exclusive-title claims, including this one, lose standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'Committer structure: this file instantiates one of three live readings of the site-legitimacy kernel; siblings are separate constraints with separate epsilon values.').

omega_variable(
    sacramental_naturalness_ambiguity,
    'Is the founding-legitimacy premise a binding normative fact (sacramental indelibility of consecration, which Orthodox canon theology treats as persistent regardless of subsequent use) or a constructed claim whose persistence serves identifiable interests?',
    'Cross-framework test: whether any governance forum outside Orthodox sacramental theology ever treats consecration as title-conferring, and whether the claim''s maintenance tracks beneficiary interest rather than doctrinal consistency.',
    'If constructed, the constraint is interest-serving performance atop a dead program, reinforcing the inertial-remnant reading; if genuinely binding within its tradition, part of its persistence is obligation rather than inertia and its retirement is theologically blocked, not merely politically blocked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_naturalness_ambiguity, conceptual, 'Whether the claim''s legitimating premise is natural-law-like within its own frame or constructed.').

omega_variable(
    enforcement_pathway_emergence,
    'Could the restitution claim acquire enforcement capacity - treaty revision, internationalization of the site, coercive diplomacy - that would convert symbolic imposition into material transfer?',
    'Track the rigidity of the Lausanne-era settlement architecture, great-power positioning on cultural-property restitution, and any Greek government action upgrading the claim from anniversary statement to negotiation objective.',
    'With a credible pathway the constraint migrates toward an enforced hybrid form with material extraction from the target seats; without one it remains an inertial remnant and the low material extraction assessment stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_pathway_emergence, empirical, 'Whether the claim''s dormancy is permanent or contingent on enforcement opportunity.').

omega_variable(
    diaspora_adherence_mechanism,
    'Is diaspora adherence to the claim sustained by community sanction (structural) or by identity fusion with the loss narrative (internalized)?',
    'Post-detachment trajectory: whether second- and third-generation members who disengage from commemorative institutions retain or shed the claim''s normative pull.',
    'If internalized, the claim outlives its institutions and the structural suppression measure understates its hold; if structural, institutional decline through assimilation and shrinking parishes will retire the claim within generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_adherence_mechanism, empirical, 'Structural versus internalized adherence mechanism in the beneficiary community.').

omega_variable(
    patriarchate_strategic_restraint,
    'Is the Ecumenical Patriarchate''s refusal to press material restitution principled (a theology of witness over title) or strategic (vulnerability to Turkish legal pressure on the see)?',
    'Counterfactual observation: Patriarchal behavior were Turkish pressure on the see relaxed - seminary reopening, property returns, legal security guarantees.',
    'If strategic, the Patriarchate is a latent agenda-setter and the claim''s dormancy is contingent, raising its extraction ceiling; if principled, the claim is permanently rhetorical regardless of enforcement opportunity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patriarchate_strategic_restraint, empirical, 'Whether the claim''s principal custodian could become an active pursuer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hs_orthodox_restitution_tr_t0, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(hs_orthodox_restitution_tr_t0, observed).
narrative_ontology:measurement(hs_orthodox_restitution_tr_t15, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(hs_orthodox_restitution_tr_t15, observed).
narrative_ontology:measurement(hs_orthodox_restitution_tr_t30, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(hs_orthodox_restitution_tr_t30, observed).
narrative_ontology:measurement(hs_orthodox_restitution_tr_t45, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 45, 0.6).
narrative_ontology:measurement_basis(hs_orthodox_restitution_tr_t45, observed).
narrative_ontology:measurement(hs_orthodox_restitution_tr_t60, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 60, 0.65).
narrative_ontology:measurement_basis(hs_orthodox_restitution_tr_t60, observed).
narrative_ontology:measurement(hs_orthodox_restitution_tr_t75, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 75, 0.68).
narrative_ontology:measurement_basis(hs_orthodox_restitution_tr_t75, observed).
narrative_ontology:measurement(hs_orthodox_restitution_tr_t90, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 90, 0.7).
narrative_ontology:measurement_basis(hs_orthodox_restitution_tr_t90, observed).
narrative_ontology:measurement(hs_orthodox_restitution_tr_t100, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 100, 0.72).
narrative_ontology:measurement_basis(hs_orthodox_restitution_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(hs_orthodox_restitution_be_t0, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(hs_orthodox_restitution_be_t0, observed).
narrative_ontology:measurement(hs_orthodox_restitution_be_t15, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement_basis(hs_orthodox_restitution_be_t15, observed).
narrative_ontology:measurement(hs_orthodox_restitution_be_t30, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(hs_orthodox_restitution_be_t30, observed).
narrative_ontology:measurement(hs_orthodox_restitution_be_t45, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 45, 0.36).
narrative_ontology:measurement_basis(hs_orthodox_restitution_be_t45, observed).
narrative_ontology:measurement(hs_orthodox_restitution_be_t60, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement_basis(hs_orthodox_restitution_be_t60, observed).
narrative_ontology:measurement(hs_orthodox_restitution_be_t75, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 75, 0.27).
narrative_ontology:measurement_basis(hs_orthodox_restitution_be_t75, observed).
narrative_ontology:measurement(hs_orthodox_restitution_be_t90, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 90, 0.29).
narrative_ontology:measurement_basis(hs_orthodox_restitution_be_t90, observed).
narrative_ontology:measurement(hs_orthodox_restitution_be_t100, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 100, 0.33).
narrative_ontology:measurement_basis(hs_orthodox_restitution_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(hs_orthodox_restitution_su_t0, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(hs_orthodox_restitution_su_t0, observed).
narrative_ontology:measurement(hs_orthodox_restitution_su_t15, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement_basis(hs_orthodox_restitution_su_t15, observed).
narrative_ontology:measurement(hs_orthodox_restitution_su_t30, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 30, 0.36).
narrative_ontology:measurement_basis(hs_orthodox_restitution_su_t30, observed).
narrative_ontology:measurement(hs_orthodox_restitution_su_t45, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 45, 0.34).
narrative_ontology:measurement_basis(hs_orthodox_restitution_su_t45, observed).
narrative_ontology:measurement(hs_orthodox_restitution_su_t60, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 60, 0.26).
narrative_ontology:measurement_basis(hs_orthodox_restitution_su_t60, observed).
narrative_ontology:measurement(hs_orthodox_restitution_su_t75, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 75, 0.22).
narrative_ontology:measurement_basis(hs_orthodox_restitution_su_t75, observed).
narrative_ontology:measurement(hs_orthodox_restitution_su_t90, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 90, 0.26).
narrative_ontology:measurement_basis(hs_orthodox_restitution_su_t90, observed).
narrative_ontology:measurement(hs_orthodox_restitution_su_t100, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 100, 0.3).
narrative_ontology:measurement_basis(hs_orthodox_restitution_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__orthodox_restitution_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The colloquial dispute over Hagia Sophia's status decomposes into three structurally distinct constraints sharing one kernel (hagia_sophia_substrate), per the epsilon-invariance principle: this orthodox_restitution_reading (title from founding consecration; beneficiaries diaspora, Greek state, patriarchate; targets Turkish sovereignty and Islamic worship continuity), the islamic_sovereignty_reading (title from conquest and endowment; beneficiaries the Turkish state and Muslim congregations; targets the Orthodox restitution claim), and the universal_heritage_reading (no exclusive title; beneficiaries conservation constituencies; targets all exclusive-claim holders). Each file authors its own epsilon, beneficiaries, and victims; the files link through affects_constraints. Historical dependency runs from the 1453 conquest settlement to all three readings; this reading's claim is downstream of the conquest fact and in direct rhetorical competition with both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, institutional, 0.25).
constraint_indexing:directionality_override(hagia_sophia_substrate__orthodox_restitution_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
