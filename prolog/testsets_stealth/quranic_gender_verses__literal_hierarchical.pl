% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Verses — Literal Hierarchical Reading (Verses 4:11, 2:282, 4:34 as Timeless Direct Legal Ordinance)
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   quranic_gender_verses: the literal-hierarchical reading, under which
 *   Qur'an 4:11 (differential inheritance shares), 2:282 (differential
 *   witness weight), and 4:34 (male guardianship and disciplinary latitude)
 *   operate as direct, timeless legal commands. The standing arrangement
 *   under contest — the ε referent — is that literal-hierarchical order as it
 *   actually operates in applying communities and courts, assessed
 *   descriptively; it is NOT the egalitarian alternative the sibling readings
 *   would install. Per the ε-invariance principle this file contains only
 *   this reading: the contextual-egalitarian and progressive-abrogation
 *   readings are separate constraints with their own ε values, victim sets,
 *   and classifications, linked through network.affects_constraints. The
 *   claim/metric gap is deliberate: the reading CLAIMS divine ordinance
 *   (which, if accepted, would behave like an unchangeable natural order),
 *   while the authored metrics describe enforced, asymmetric operation with
 *   identifiable gainers and bearers — the engine measures that divergence
 *   rather than the author reconciling it.
 *
 * KEY AGENTS:
 *   - - male_household_heads: Primary beneficiary (organized/arbitrage) — collects preferential shares, guardianship authority, and disciplinary latitude; exits cheaply to secular orders
 *   - - religious_court_establishment: Agenda setter (institutional/identity_locked) — administers estate division, testimony weighting, and marriage law; its office is constituted by the reading it enforces
 *   - - female_heirs: Primary target (powerless/trapped) — bears reduced shares, discounted testimony, mediated property claims
 *   - - wives_under_marital_discipline: Primary target (powerless/trapped) — bears guardianship authority and asymmetric divorce access
 *   - - muslim_community_members: Coordination participant (moderate/constrained) — receives succession predictability and contract standardization, carries misfit costs
 *   - - reformist_hermeneuts: Excluded voice (moderate/mobile) — barred from adjudication, publishes outside the court system
 *   - - national_family_law_legislators: Analytical observer (institutional/analytical) — decides codification extent, takes testimony from all seats
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.8).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.85).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.8).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses — Literal Hierarchical Reading (Verses 4:11, 2:282, 4:34 as Timeless Direct Legal Ordinance)").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'a121d440-4e13-465c-b9a7-81f1d658a809').
narrative_ontology:cs_kernel_codification('a121d440-4e13-465c-b9a7-81f1d658a809', fixed_text).
narrative_ontology:cs_authority_grounding('a121d440-4e13-465c-b9a7-81f1d658a809', lineage).
narrative_ontology:cs_interpretation_layer_present('a121d440-4e13-465c-b9a7-81f1d658a809').
narrative_ontology:cs_reading_relation('a121d440-4e13-465c-b9a7-81f1d658a809', quranic_gender_verses__contextual_egalitarian, forecloses).
narrative_ontology:cs_reading_relation('a121d440-4e13-465c-b9a7-81f1d658a809', quranic_gender_verses__progressive_abrogation, forecloses).
narrative_ontology:cs_axiom('a121d440-4e13-465c-b9a7-81f1d658a809', foundational, explicit_ahkam_bind_timelessly).
narrative_ontology:cs_axiom_status(explicit_ahkam_bind_timelessly, holdable).
narrative_ontology:cs_axiom_grounding('a121d440-4e13-465c-b9a7-81f1d658a809', explicit_ahkam_bind_timelessly, theological).
narrative_ontology:cs_axiom('a121d440-4e13-465c-b9a7-81f1d658a809', secondary, male_qiwama_orders_household_reciprocity).
narrative_ontology:cs_axiom_status(male_qiwama_orders_household_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('a121d440-4e13-465c-b9a7-81f1d658a809', male_qiwama_orders_household_reciprocity, theological).
narrative_ontology:cs_reference_frame('a121d440-4e13-465c-b9a7-81f1d658a809', timeless_direct_divine_command).
narrative_ontology:cs_drift_state('a121d440-4e13-465c-b9a7-81f1d658a809', contemporary_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a121d440-4e13-465c-b9a7-81f1d658a809', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_court_establishment).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_heirs).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, wives_under_marital_discipline).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, muslim_community_members).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, muslim_community_members).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, timeless_binding_force_of_explicit_ahkam).
narrative_ontology:constraint_vindicates(quranic_gender_verses__literal_hierarchical, divine_wisdom_of_gender_differentiated_allocation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive preferential inheritance portions in most kinship configurations, hold guardianship authority over female relatives' marriage, travel, and legal affairs, and exercise disciplinary latitude within marriage. Owe maintenance obligations in exchange. Enforce the norms informally inside families and through community standing. If they relocate to a jurisdiction applying secular family law, they lose little status and can continue benefiting from communal deference — their position survives either legal regime.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, male_household_heads, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, male_household_heads, agenda_setter).

% Judges, jurists, and muftis who divide estates according to the fixed shares, assign evidentiary weight to witnesses, register marriages under guardianship requirements, and adjudicate marital discipline cases. Their interpretive authority, offices, and livelihoods are constituted by upholding the verses' direct, unmediated legal force; abandoning that position would dissolve the basis of their own office. The institution has become its function.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, religious_court_establishment, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Receive half-share portions in many inheritance configurations and often require a male intermediary to claim or manage what they receive. Contesting a division risks family rupture and community sanction. Their testimony counts as half a man's in financial matters, shaping what property claims they can press. Leaving the community through apostasy or estrangement carries severe legal and social cost, so the arrangement is not something they can walk away from.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, female_heirs, payer,
    powerless, biographical, trapped, global).

% Live under a husband's authority that includes the disciplinary latitude of verse 4:34, face higher barriers to initiating divorce than he faces, and lose maintenance and often custody upon separation. Community and court enforcement backs the husband's position. Exit routes run through family rupture, economic precarity, and in some jurisdictions criminal exposure.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, wives_under_marital_discipline, payer,
    powerless, biographical, trapped, global).

% Receive predictable succession rules, standardized witnessing for financial contracts, and clearly assigned household responsibilities — settled answers to problems that would otherwise be negotiated case by case. Also carry the costs where the fixed rules misfit contemporary commerce, education, and dual-income households. Exiting means leaving communal religious life itself, which most members will not do.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, muslim_community_members, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__literal_hierarchical, muslim_community_members, payer).

% Scholars arguing that the verses are historically situated and must be reread through overriding equity principles. They publish through universities, research centers, and diaspora institutions but are barred from official adjudication in jurisdictions applying the literal reading, and some face accusations of unbelief. Their exclusion from the courtroom is maintained by the same interpretive authority the arrangement rests on.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, reformist_hermeneuts, excluded,
    moderate, generational, mobile, global).

% Legislators and senior judges in Muslim-majority states deciding how much of the literal reading to codify. They weigh pressure from the religious establishment against reform constituencies and international treaty commitments; some states have codified substantial modifications to succession and divorce law, others enforce the literal reading in full. They take testimony from every other seat and can alter the arrangement's legal force within their borders.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__literal_hierarchical, national_family_law_legislators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:fixing_cost_class(quranic_gender_verses__literal_hierarchical, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles succession shares predictably across an extended kinship system, standardizes evidentiary weight for financial dealings in low-documentation economies, and assigns maintenance responsibility and decision authority within households — solving, once and centrally, problems of disputed estates, unenforceable debt claims, and household governance.
% TRANSFER_FUNCTION: Moves inheritance wealth (preferential male portions), courtroom credibility (differential witness weight), and household decision power from women to male kin, and moves adjudicative authority and interpretive office to the religious court establishment.
% ABSENT_VOICES: Women subject to the rules were largely absent from the classical tafsir and fiqh canon-formation councils where the reading was consolidated, and remain absent from bench and academy in literalist jurisdictions today; reformist hermeneuts are excluded from official adjudication entirely. The unanimity of the classical consensus partly reflects who was never in the room.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight in applying jurisdictions, estate divisions would redistribute immediately toward equal shares, marriage registration and divorce procedure would reorganize around symmetrical standing, courtroom evidence rules would change, and the religious courts' jurisdiction over family matters would contract sharply — the family-law order of entire legal systems would rearrange.
% FOUNDING_PROBLEM: Seventh-century Arabian estate disputes amid clan restructuring and mass conversion, unreliable debt documentation when few women handled commerce, and the need for stable household order in a transitional tribal society.
% FOUNDING_PROBLEM_CORROBORATION: Academic historiography of pre-Islamic and early Islamic Arabia — scholarship produced outside the beneficiary set — corroborates the situational character of the original problems (documented inheritance conflict, thin commercial paper, clan dissolution). The religious establishment attests instead that the ordinance addresses permanent features of human nature and remains fully live. Notably, corroboration for the still-live claim comes almost exclusively from inside the benefiting parties; that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(quranic_gender_verses__literal_hierarchical, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__literal_hierarchical, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__literal_hierarchical, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quranic_gender_verses__literal_hierarchical, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__literal_hierarchical, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80 at interval end) because the transfers are fixed by text and decoupled from circumstance: the shares, the witness discount, and the guardianship default apply regardless of the parties' actual capacities, and the differential has grown more costly as women's economic and educational position diverged from the seventh-century baseline. Suppression (0.85) reflects the exit structure: apostasy costs, family rupture, custody and maintenance exposure, and court enforcement of guardianship — the arrangement persists through closed exits more than through consent. Theater ratio is low-to-moderate (0.28): estate division and contract witnessing are really performed functions, though a growing share of enforcement activity defends the interpretive monopoly rather than adjudicating disputes. Accessibility_collapse is moderate (0.55): within a applying jurisdiction the fixed shares cannot be contracted around beyond narrow wasiyya limits, but interpretive and jurisdictional alternatives persist at the margins. Resistance (0.60) is real and organized — reform movements, feminist hermeneutics, and statutory reform in several states. The suppression series is deliberately CYCLICAL, not monotonic: enforcement machinery consolidates during institution-building and revivalist waves (classical fiqh codification circa 900, post-Mongol reconstruction circa 1258, the post-1979 revival) and relaxes under cosmopolitan-imperial and secularizing pressure (late Ottoman reform, 1924). The oscillation is partly functional to persistence — periodic intensification reasserts the interpretive monopoly after each relaxation, an intermittent-reinforcement dynamic — and partly exogenous (imperial capacity, state formation). All three metric series are authored on one shared seven-point grid (632, 900, 1258, 1800, 1924, 1979, 2026) so the engine samples complete rows; endpoint values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute very differently. From the religious court establishment's position the arrangement is a divinely given order it faithfully administers — coordination and obedience, not taking. From the trapped payer seats the same structure operates as enforced transfer of wealth, credibility, and autonomy. Male household heads occupy a third position: net gainers with cheap exit, for whom the arrangement is a favorable default they never had to fight for. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Female_heirs and wives_under_marital_discipline sit nearest the full-target end: they bear the transfers directly and their exits are trapped (apostasy, rupture, economic exposure), so effective extraction is amplified. Male_household_heads sit near the beneficiary end: they collect the preferential shares and authority, and their arbitrage-grade exit (secular jurisdictions, private reinterpretation) means the arrangement subsidizes them under either regime. Religious_court_establishment derives low d from its beneficiary position but is the enforcement principal — its identity_locked exit means the institution cannot abandon the reading without dissolving itself, which stabilizes enforcement regardless of individual belief. Muslim_community_members sit near symmetric: genuine coordination goods received, misfit costs paid diffusely. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and the global scope of the arrangement (verification of fair application across the umma is hard, modestly amplifying effective extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — estate dispute, thin debt documentation, household order in a clan-transition society — are partially transformed rather than dead: modern notarization and formal credit have dissolved the documentary rationale for the witness-weighting rule, while estate complexity and household governance disputes persist in new forms. Because the status is contested rather than plainly dead, the mismatch consumer reads (contested x world_rearranges) and no zombie flag is asserted. The classification prevents two opposite mislabels: calling this a pure snare would erase the genuine coordination function (succession predictability and contract standardization are real services the community would miss, and the contextual-egalitarian sibling proposes a coordinated alternative rather than abolition); calling it a rope would erase the asymmetric, enforced transfer that defines its operation. Tangled_rope holds both halves: coordinated AND extractive through the same structure, held in place by active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel quranic_gender_verses (reading: literal_hierarchical). What structurally changes if a sibling reading governs instead?',
    'Comparative institutional analysis of jurisdictions applying different readings of the same verses: succession outcomes, testimony rules, guardianship requirements, and women''s property participation under literalist versus reformed family codes.',
    'Under contextual_egalitarian or progressive_abrogation the victim set contracts or dissolves, male resource concentration falls toward parity, and epsilon drops sharply — likely reclassifying the sibling stories below the tangled-rope extraction threshold. The disagreement is located in the mediating premise: whether historical context and later principles stand BETWEEN the text and its legal force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is the literal_hierarchical reading of a three-reading kernel; sibling readings instantiate different constraints.').

omega_variable(
    divine_timelessness_vs_constructed_benefit,
    'Is this arrangement an unchangeable divine ordinance (which would behave like a fixed natural order with zero degrees of freedom and no collecting party), or a constructed legal arrangement whose persistence benefits identifiable agents?',
    'Observe whether the arrangement''s application tracks the text alone or tracks the interests of its administrators: variance in application across jurisdictions and eras under identical text, and the establishment''s response to interpretive challenge, reveal whether enforcement serves the ordinance or the enforcers.',
    'If the divine-timeless framing is accepted wholesale, the constraint approaches mountain-like immutability and beneficiary declarations become category error; if constructed, the declared beneficiaries and victims stand and the tangled-rope classification holds. The FSM signature evaluates exactly this ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_timelessness_vs_constructed_benefit, conceptual, 'Natural-law versus constructed-constraint ambiguity at the heart of the literalist claim.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of women under this arrangement structural (legal barriers, court enforcement, economic dependency) or internalized (piety-formed acceptance, identity fusion with the divinely ordained order)?',
    'Post-exit suppression trajectory: track women who exit applying jurisdictions or undergo conversion/estrangement — if deference patterns and self-limitation persist after the enforcement machinery is removed, a substantial internalized component exists.',
    'If largely internalized, effective suppression exceeds the structural measure and outlives legal reform — statutory change alone would not release the targets; if largely structural, jurisdictional reform translates directly into changed outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a high-suppression interpersonal-legal arrangement.').

omega_variable(
    womens_coalition_power_conversion,
    'Can women, individually powerless under the arrangement, convert class position into organized power sufficient to alter enforcement (as reform movements and statutory campaigns have attempted)?',
    'Track outcomes of collective action episodes: Moroccan moudawana reform, Tunisian succession-law proposals, transnational feminist hermeneutics networks — did organized pressure change codified application, and did the establishment absorb or reverse the changes?',
    'Successful conversion would move the payer seats from powerless toward organized, damping effective extraction over time and potentially driving lifecycle drift toward scaffold-like transition; failure would confirm the exit-and-coalition closure that stabilizes the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(womens_coalition_power_conversion, empirical, 'Whether coalition dynamics can restructure the power atoms of the trapped payer seats.').

omega_variable(
    jurisdictional_epsilon_variance,
    'The same reading produces radically different legal regimes across jurisdictions (full enforcement versus heavily modified codification). Is this one constraint with jurisdiction-dependent application, or a family of jurisdictional instantiations each deserving its own story?',
    'Test ε-invariance directly: if measuring the arrangement in a fully enforcing jurisdiction versus a reformed one yields materially different epsilon for the SAME reading, decompose into per-jurisdiction stories linked by network edges; if the variance is attributable to sibling readings quietly governing in reformed jurisdictions, the single-story span is correct.',
    'Decomposition would sharpen every downstream computation (per-jurisdiction directionality, scope-scaled extraction); failure to decompose leaves the authored 0.80 as a weighted blend that matches no single courtroom exactly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_epsilon_variance, conceptual, 'Scope-of-instantiation ambiguity: one reading-spanning story versus a jurisdictional constraint family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 632, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t632, quranic_gender_verses__literal_hierarchical, theater_ratio, 632, 0.1).
narrative_ontology:measurement(qura_tr_t900, quranic_gender_verses__literal_hierarchical, theater_ratio, 900, 0.12).
narrative_ontology:measurement(qura_tr_t1258, quranic_gender_verses__literal_hierarchical, theater_ratio, 1258, 0.15).
narrative_ontology:measurement(qura_tr_t1800, quranic_gender_verses__literal_hierarchical, theater_ratio, 1800, 0.18).
narrative_ontology:measurement(qura_tr_t1924, quranic_gender_verses__literal_hierarchical, theater_ratio, 1924, 0.22).
narrative_ontology:measurement(qura_tr_t1979, quranic_gender_verses__literal_hierarchical, theater_ratio, 1979, 0.25).
narrative_ontology:measurement(qura_tr_t2026, quranic_gender_verses__literal_hierarchical, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t632, quranic_gender_verses__literal_hierarchical, base_extractiveness, 632, 0.58).
narrative_ontology:measurement(qura_be_t900, quranic_gender_verses__literal_hierarchical, base_extractiveness, 900, 0.62).
narrative_ontology:measurement(qura_be_t1258, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1258, 0.65).
narrative_ontology:measurement(qura_be_t1800, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1800, 0.68).
narrative_ontology:measurement(qura_be_t1924, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1924, 0.72).
narrative_ontology:measurement(qura_be_t1979, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1979, 0.77).
narrative_ontology:measurement(qura_be_t2026, quranic_gender_verses__literal_hierarchical, base_extractiveness, 2026, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t632, quranic_gender_verses__literal_hierarchical, suppression_requirement, 632, 0.5).
narrative_ontology:measurement(qura_su_t900, quranic_gender_verses__literal_hierarchical, suppression_requirement, 900, 0.66).
narrative_ontology:measurement(qura_su_t1258, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1258, 0.74).
narrative_ontology:measurement(qura_su_t1800, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(qura_su_t1924, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1924, 0.52).
narrative_ontology:measurement(qura_su_t1979, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1979, 0.76).
narrative_ontology:measurement(qura_su_t2026, quranic_gender_verses__literal_hierarchical, suppression_requirement, 2026, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, attachment_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Qur'anic gender verses' conflates three structurally distinct constraints corresponding to three readings of one kernel. This file is the literal_hierarchical member (highest ε: enforced asymmetric transfer with trapped targets). The upstream member in citation order is contextual_egalitarian (same verses as situated steps — lower ε, different victim set), and progressive_abrogation stands downstream (verses superseded by later principles — lowest ε of the family). The literal reading is cited BY the other two as the position to be overcome, so this story influences both siblings' legitimacy conditions even while being logically incompatible with each within a single hermeneutic framework. Each member carries its own ε, beneficiaries, victims, and classification; none averages across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
