% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: Charter's Islamic-Nationalist Sovereign Legitimacy Framework
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A post-revolutionary state adopts a charter that grounds its sovereign
 *   legitimacy in Islamic-nationalist identity and religious law, displacing
 *   the secular constitutional frameworks and institutions of the ancien
 *   régime. This constraint story instantiates the GUIDED-NATIONALISM reading
 *   of the contested kernel 'july_charter_sovereign_legitimacy'—the reading
 *   that holds the Charter's religious-identity ground as the legitimate,
 *   authentic foundation of post-revolutionary governance. Sibling readings
 *   frame the same Charter as military custodianship (the military as
 *   permanent guardian) or as mandating secular democratic institutions
 *   (civilians over the armed forces). These readings are not about different
 *   constitutional texts; they are different framings of the same text's
 *   legitimacy source. The guided-nationalism reading asserts that religious
 *   identity IS that source and that secular institutions are rightly
 *   subordinated to it. This reading benefits the religious-nationalist elite
 *   and the state apparatus claiming Islamic mandate; it victimizes secular
 *   civil society, religious minorities, and women operating under
 *   pre-Charter civil law. The measurement series show extraction rising
 *   steeply in the first 15 years (as religious law is consolidated and
 *   secular institutions are subordinated), then plateauing as the new order
 *   stabilizes.
 *
 * KEY AGENTS:
 *   - religious_nationalist_elite: Drafters and interpreters of the Charter's religious-legitimacy frame (institutional power; arbitrage exit via control of interpretation)
 *   - state_apparatus_claiming_islamic_mandate: Security, bureaucratic, and judicial machinery operating under religious sanction rather than civilian oversight (institutional power; arbitrage exit via monopoly on force)
 *   - secular_civil_society: Lawyers, judges, educators, journalists pre-Charter era (organized power; constrained exit via institutional displacement)
 *   - religious_minorities: Non-Muslim and non-majority-Islamic populations (powerless; identity-locked exit via citizenship-status loss)
 *   - women under religious law: Operating under religious-law personal-status regimes (moderate power; identity-locked exit via guardianship and inheritance law)
 *   - former secular constitutional order: The institutional and legal structure being overwritten (excluded, non-agent; trapped exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.72).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "Charter's Islamic-Nationalist Sovereign Legitimacy Framework").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'f45b8864-565c-4e96-961f-cc7828217c3b').
narrative_ontology:cs_kernel_codification('f45b8864-565c-4e96-961f-cc7828217c3b', formalized).
narrative_ontology:cs_authority_grounding('f45b8864-565c-4e96-961f-cc7828217c3b', lineage).
narrative_ontology:cs_interpretation_layer_present('f45b8864-565c-4e96-961f-cc7828217c3b').
narrative_ontology:cs_reading_relation('f45b8864-565c-4e96-961f-cc7828217c3b', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_reading_relation('f45b8864-565c-4e96-961f-cc7828217c3b', july_charter_sovereign_legitimacy__secular_democratic_reading, influences).
narrative_ontology:cs_axiom('f45b8864-565c-4e96-961f-cc7828217c3b', foundational, islamic_identity_as_sovereign_foundation).
narrative_ontology:cs_axiom_status(islamic_identity_as_sovereign_foundation, holdable).
narrative_ontology:cs_axiom_grounding('f45b8864-565c-4e96-961f-cc7828217c3b', islamic_identity_as_sovereign_foundation, deontological).
narrative_ontology:cs_axiom('f45b8864-565c-4e96-961f-cc7828217c3b', foundational, religious_law_supremacy_over_secular_institutions).
narrative_ontology:cs_axiom_status(religious_law_supremacy_over_secular_institutions, holdable).
narrative_ontology:cs_axiom_grounding('f45b8864-565c-4e96-961f-cc7828217c3b', religious_law_supremacy_over_secular_institutions, conventional).
narrative_ontology:cs_axiom('f45b8864-565c-4e96-961f-cc7828217c3b', secondary, guided_nationalism_against_western_individualism).
narrative_ontology:cs_axiom_status(guided_nationalism_against_western_individualism, holdable).
narrative_ontology:cs_axiom_grounding('f45b8864-565c-4e96-961f-cc7828217c3b', guided_nationalism_against_western_individualism, empirically_contingent).
narrative_ontology:cs_reference_frame('f45b8864-565c-4e96-961f-cc7828217c3b', post_revolutionary_islamic_nationalist_legitimacy).
narrative_ontology:cs_drift_state('f45b8864-565c-4e96-961f-cc7828217c3b', contemporary_post_charter_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f45b8864-565c-4e96-961f-cc7828217c3b', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_elite).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_apparatus_claiming_islamic_mandate).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, civilian_democratic_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, women_under_religious_law_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls constitutional text and its interpretation. Authors the Charter's religious-legitimacy framing; claims to speak for the nation's spiritual foundation and authentic identity against colonial secularism. Collects institutional authority and resource allocation through this frame. Interprets religious law and determines which secular norms are permissible.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Bureaucratic, security, and administrative machinery that derives legitimacy from the Charter's religious-nationalist framing. Uses the framework to suppress secular institutional competition and consolidate centralized authority. Operates armed forces, intelligence services, and administrative courts under religious sanction rather than civilian oversight.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_apparatus_claiming_islamic_mandate, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_apparatus_claiming_islamic_mandate, agenda_setter).

% Lawyers, judges, educators, journalists, and civic organizations built under secular constitutional frameworks prior to the Charter. Faces subordination of civil law to religious interpretation; their professional autonomy is constrained by religious-authority claims. Courts they staffed are overridden; associations they formed are regulated by religious compatibility tests.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    organized, biographical, constrained, national).

% Christians, Baha'is, Zoroastrians, atheists, and non-majority Islamic schools. Lose legal standing and citizenship rights under a Charter that constitutionalizes majoritarian religious identity as the state's foundation. Their worship, speech, and civic participation are restricted by religious-law provisions; exit means literal displacement or forced conversion.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, biographical, identity_locked, national).

% Face personal-status law (marriage, inheritance, testimony, guardianship) derived from religious-nationalist interpretation of Islamic law. The Charter's religious-legitimacy ground overrides secular family law that had expanded their legal autonomy. Witness testimony may be discounted; guardianship requirements constrain independent action; inheritance shares may be unequal. Exit requires physical departure from jurisdiction.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, women_under_religious_law_interpretation, payer,
    moderate, biographical, identity_locked, national).

% The pre-Charter institutional and legal structure—constitutional courts, civil codes, secular educational frameworks—is not a party but is the displaced referent. Its authority is overwritten, not negotiated with; its institutions are subordinated or dissolved.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, former_secular_constitutional_framework, excluded,
    powerful, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(july_charter_sovereign_legitimacy__guided_nationalism_reading, former_secular_constitutional_framework).

% Political factions advocating for secular, internationally-integrated constitutionalism are barred from the Charter's interpretive authority and from framing legitimacy claims. They may exist but are cast as un-Islamic, foreign-influenced, and illegitimate; the Charter's religious-supremacy provision forecloses their institutional claims.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, western_aligned_secular_democrats, excluded,
    organized, biographical, constrained, national).

% International human-rights bodies, foreign governments, and comparative constitutional scholars observe and may monitor compliance with secular-liberal norms. They have no standing in Charter interpretation but may apply diplomatic or legal pressure; they are explicitly external to the legitimacy frame.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_secular_observers, observer,
    powerful, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_elite).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative source of constitutional legitimacy grounded in religious identity and law, solving the post-revolutionary state-building problem of 'who has authority to govern and on what grounds' by anchoring authority in the nation's claimed authentic (religious-nationalist) identity against both colonial secularism and factional pluralism.
% TRANSFER_FUNCTION: Transfers constitutional authority, legal standing, and institutional resources from secular institutions and religious minorities to a unified state apparatus operating under religious-nationalist legitimacy; moves women's family-law autonomy to religious-law regimes; moves civil-society independence to state-supervised Islamic-compatibility frameworks.
% ABSENT_VOICES: Secular democrats, religious minorities, women's-rights advocates, and the constituencies of the displaced pre-Charter secular order are structurally excluded from the frame that legitimates the new constitution. Their objections are pre-judged as foreign, un-Islamic, or illegitimate by definition. International human-rights observers have only external commentary, not standing.
% DISAPPEARANCE_RATIONALE: If the Charter and its religious-nationalist legitimacy frame vanished, competing institutional claims (secular courts, military custodianship, pluralist democracy, international constitutionalism) would contest the state's foundation immediately. The state apparatus would lose its claimed religious sanction; authority would devolve or reorganize into competing frameworks. Secular law would re-emerge in courts; women's personal-status law might revert to pre-Charter civil codes; religious minorities might recover civic standing.
% FOUNDING_PROBLEM: Post-revolutionary state-building after displacement of a secular, foreign-aligned regime. The legitimacy vacuum: what principle should ground the new state's authority? Revolutionary factions competed over whether the answer was Islam, military stability, democratic consent, or national development. The religious-nationalist reading answered: Islamic identity as the authentic foundation, rejected by the ancien régime.
% FOUNDING_PROBLEM_CORROBORATION: The religious-nationalist elite and state apparatus claim the founding problem is perennially live: constant threat of Western infiltration and secularization. Secular democrats and international observers attest the problem was a specific historical-political choice (the revolutionary moment), not a permanent structural fact; they argue the legitimacy of post-revolutionary governance can be settled through democratic process or secular constitutionalism instead. The contestation itself—whether religious identity is a timeless foundation or a contingent historical choice—is the unresolved reading difference. No external corroboration exists that settles which reading is correct; the problem's status is the object of the constitutional contest itself.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over 30 years because the religious-nationalist legitimacy frame initially competes with multiple institutional claims (secular courts, democratic voices, military autonomy) and gradually concentrates authority in the state apparatus operating under religious sanction. Early years show lower extraction partly because implementation is incomplete; later plateauing reflects a stabilized order where the frame is consolidated and resistance is either co-opted or suppressed. Suppression rises from 0.48 to 0.72 over the same interval, tracking the enforcement machinery required to exclude secular institutional competition and maintain religious-law supremacy. Theater rises from 0.20 to 0.41: the Charter's stated function (solving the post-revolutionary legitimacy vacuum) remains real, but an increasing share of enforcement activity is devoted to defending religious-supremacy exclusivity rather than the coordination problem itself. Accessibility collapse is 0.65 because secular alternatives (democratic constitution, secular courts, international law) remain theoretically available but are pre-foreclosed by the religious-nationalist frame as foreign, un-Islamic, illegitimate—collapse is high but not total because exit toward these alternatives remains possible (migration, international venue, underground organizing) even if structurally discouraged. Resistance is 0.58: secular constituencies and minorities mount real resistance (legal challenges, civil disobedience, diaspora organization), but the state apparatus's monopoly on interpretive authority and force limits their success.
 *
 * PERSPECTIVAL GAP:
 *   The guided-nationalism reading and the secular-democratic reading should compute very differently from the engine. From the religious-nationalist seat, the arrangement is genuine coordination solving a real post-revolutionary legitimacy problem; extraction is the price of authentic governance against foreign influence. From the secular-democratic seat, the same constraint is enforced subordination of democratic institutions to a religious-monopoly frame that benefits a specific elite faction. From the military-custodian reading's seat, the constraint subordinates both religious authority and democratic process to the armed forces' stabilization function. The engine computes each seat's type from power and exit options: secular democrats are organized with constrained exit (organized power, constrained exit = moderate d in the 0.40-0.60 range, placing them closer to targets); religious minorities are powerless with identity-locked exit (powerless, identity-locked = very high d, near full targets); the agenda-setting elite are institutional with arbitrage exit (institutional power, arbitrage exit = low d, near beneficiaries). These derivations should produce divergent per-seat classifications without any seat overriding the others' typology—each sees the same constraint through its structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious-nationalist elite and state apparatus have low directionality (d near 0.0-0.25): they control interpretation, set the rules, and directly collect the institutional resources (administrative authority, legal standing, resource allocation) that the constraint creates. Exit for them is arbitrage—they can reframe or reinterpret the Charter's religious-law provisions as governance facts change, maintaining their position. Secular civil society has high directionality (d in the 0.55-0.75 range): they are organized, can mobilize collectively, but face constrained exit—leaving the jurisdiction is possible but costly (career displacement, institutional dissolution). Their options for secular legal practice, democratic politics, or civil-society independence are foreclosed within the Charter's frame, but alternatives exist internationally. Religious minorities have the highest directionality (d in the 0.80-1.0 range): they are powerless individually, their exit is identity-locked (conversion or displacement), and the constraint directly strips their legal standing and citizenship rights. No institutional power or external arbitrage option is available. Women under religious law have moderate-to-high directionality (d in the 0.65-0.85 range): they are moderate in organized power but identity-locked via family law and guardianship requirements; exit requires literal geographic displacement. These directionality assignments feed the engine's computation of effective extraction per seat: beneficiaries experience negative extraction (subsidy), targets experience extraction amplified by their trapped status and the constraint's large scope (national).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimacy source for post-revolutionary governance) remains contested. The religious-nationalist reading claims it is live: constant threat of secularization and Western influence requires ongoing religious-nationalist vigilance. Secular observers and displaced institutions claim the problem was a contingent historical choice during the revolutionary window; they argue modern legitimacy can rest on democratic consent or secular constitutionalism instead. The mandatrophy tension is real: if the founding problem has died (secular governance is no longer existentially threatening, or democratic legitimacy proves workable), the Charter persists as pure extraction disguised as coordination. If the founding problem remains live (religious identity IS the necessary foundation for post-colonial state stability), the extraction is the unavoidable cost of solving that problem. The measurement series show rising theater_ratio (rising performative enforcement of religious-supremacy exclusivity), which suggests mandatrophy drift: increasingly, enforcement is devoted to maintaining the religious-nationalist frame itself rather than solving the coordination problem it claims to solve. This drift supports the secular reading's claim that the founding problem has been artificially extended past its historical moment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_identity_as_timeless_foundation_or_historical_choice,
    'Is Islamic identity a timeless, necessary foundation for post-revolutionary state legitimacy, or a contingent historical choice made during a specific revolutionary moment?',
    'Post-Charter drift analysis: if successive political transitions (new leadership, demographic change, international integration) renegotiate the religious-nationalist frame without state dissolution, the identity ground is revisable (historical choice). If the frame persists despite such pressures or reasserts itself when challenged, it may indicate perceived structural necessity.',
    'If historical choice: the founding problem has died and the constraint''s extraction becomes mandatrophy (zombie extraction). If structural necessity: the extraction is the cost of solving a perennial legitimacy problem. This is the core ambiguity that distinguishes the guided-nationalism reading from the secular-democratic reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_identity_as_timeless_foundation_or_historical_choice, conceptual, 'Whether the Charter''s religious-identity ground is a timeless foundation or a bounded historical decision.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) primarily structural (legal prohibition, institutional exclusion, economic barriers) or internalized (religious minorities and seculars have adopted the religious-nationalist frame''s legitimacy, making their opposition to it psychologically fractured)?',
    'Post-constraint trajectory: if suppression persists after the constraint is removed (diaspora communities retaining religious-nationalist identity, returnees re-adopting it), the suppression is partly internalized. If suppression dissipates post-removal, it was primarily structural.',
    'If structural: the constraint''s extractiveness can be reduced by removing legal barriers and restoring institutional pluralism. If internalized: the constraint has reshaped constituencies'' self-understanding; exit requires identity reconstruction and may face post-exit suppression persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in religious-nationalist identity lock.').

omega_variable(
    secular_democracy_alternative_availability,
    'Is secular democratic governance available as a genuine institutional alternative within the bounded national context, or does post-colonial geopolitics (international pressure, regional religious movements, security threats) make secular governance structurally unstable?',
    'Regional comparative analysis: do neighboring or similarly-positioned post-colonial states implement secular constitutionalism with durable stability? Does this state''s historical record show secular governance collapsing under specific internal/external pressures?',
    'If secular democracy is structurally available: the religious-nationalist frame is a choice, not a necessity, and mandatrophy is likely. If secular governance is consistently unstable in this region: the religious-nationalist frame solves a real coordination problem and extraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_democracy_alternative_availability, empirical, 'Structural availability of secular-democratic governance as alternative to religious-nationalist legitimacy.').

omega_variable(
    religious_minorities_exit_availability,
    'Is exit (displacement, conversion, migration) practically available to religious minorities, or is the identity_locked status near-total because exit costs exceed all alternatives?',
    'Demographic tracking: do religious minorities migrate, convert, or accept subordination? What are the revealed costs of each path? Do international resettlement pathways exist and are they used?',
    'If exit is available at moderate cost: directionality for minorities drops from 0.95 toward 0.75-0.85, reducing effective extraction. If exit is near-impossible: directionality stays near 1.0 and minorities sit as full targets of this constraint''s extractive machinery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(religious_minorities_exit_availability, empirical, 'Practical availability of exit options for religious minorities under the religious-nationalist Charter.').

omega_variable(
    women_personal_status_law_renegotiation,
    'Is women''s subordination under religious-law personal-status regimes a constitutive feature of the religious-nationalist identity frame, or can the guided-nationalism reading be sustained while renegotiating personal-status law toward civil-law equality?',
    'Jurisprudential evolution: do successive interpretations of the Charter''s religious-law provisions expand women''s rights (guardianship waiver, equal inheritance, independent testimony) while maintaining the religious-nationalist legitimacy frame? Can the religious-nationalist elite concede women''s equality without forfeiting their claim to authentic Islamic governance?',
    'If personal-status subordination is negotiable: women''s directionality can shift from 0.70-0.85 toward 0.50-0.70, reducing their effective extraction and weakening snare classification. If it is constitutive: women are locked into high-d targets and remain a core victim set of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_personal_status_law_renegotiation, conceptual, 'Whether religious-nationalist identity requires women''s subordination or permits civil-law equality in personal status.').

omega_variable(
    secular_democratic_reading_foreclosure,
    'Does the guided-nationalism reading logically foreclose the secular-democratic reading (two readings cannot coexist in one framework), or do they coexist as different parties'' live claims (multiple frameworks hold both simultaneously)?',
    'Jurisprudential analysis: can a coherent Charter interpretation hold BOTH religious identity as the legitimacy source AND democratic institutions as supreme? Or does recognizing one require denying the other?',
    'If foreclosed: the reading relation is ''forecloses'' and the two readings are in structural contradiction. If coexisting: they are held by different factions and the relation is ''coexists_with'', indicating a live constitutional contest rather than a resolved principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_democratic_reading_foreclosure, conceptual, 'Logical relationship between guided-nationalism and secular-democratic readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.14).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'july_charter_sovereign_legitimacy'. The guided-nationalism reading holds that the Charter grounds sovereignty in Islamic-nationalist identity and religious law. The military-custodian reading holds that it grounds sovereignty in the armed forces as permanent stabilizer. The secular-democratic reading holds that it mandates secular institutions with civilian supremacy. All three are readings of the same Charter text; each produces a different constraint with different beneficiary/victim structures, different directionality assignments, and different extracted surplus flows. The three constraints are structurally linked by network.affects_constraints because challenge to one reading (e.g., evidence that religious-nationalist legitimacy is unstable or contested) creates structural pressure on the others' viability. Decomposition follows the ε-invariance principle: each reading has a different ε (different referent—what is being extracted/coordinated under that reading's lights), different beneficiaries, and different victim sets. A single constraint cannot hold all three readings without violating ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__guided_nationalism_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
