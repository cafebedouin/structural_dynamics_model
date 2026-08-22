% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Contact Language for Diaspora Cohesion
 *   domain: sociolinguistics/identity_coordination
 *
 * SUMMARY:
 *   Hebrew lives in diaspora Jewish communities as a contact language for
 *   intra-communal coordination: a standardized, non-native variety that
 *   combines high-register written/liturgical elements, loanwords from local
 *   majority languages, and simplified morpho-syntax from marketplace
 *   interaction. It is neither the liturgical Hebrew of classical texts nor
 *   the generative native speech of Israeli speakers. The bridge-pidginized
 *   reading frames this as a pragmatic, instrumental accomplishment — a
 *   legitimate language form that solves community cohesion in linguistically
 *   fragmented diaspora. Both the liturgical-preservation reading (which
 *   dismisses it as 'not really Hebrew' because it lacks textual authority)
 *   and the native-generative reading (which dismisses it as 'not really
 *   Hebrew' because it lacks native-speaker foundations) reject the
 *   bridge-pidginized framing. This story instantiates the bridge-pidginized
 *   reading ONLY — the structural accommodation that persists despite both
 *   competing frames. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as tangled_rope (coordination function + asymmetric extraction)
 *   while the authored metrics show rising extractiveness over the interval
 *   (theater_ratio rising from 0.22 to 0.41 suggests institutional
 *   maintenance increasingly overtakes spontaneous community use). The engine
 *   measures this divergence.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: primary coordinating collective (organized, constrained exit) — benefit from common code but bear cost of pidginized register
 *   - hebrew_educators: institutional agents (institutional, constrained exit) — set and enforce the bridge-pidginized standard
 *   - heritage_language_learners: moderate-power beneficiaries of coordination but payers via non-transferable skills (moderate, constrained exit)
 *   - native_speaker_aspirants: powerless victims of identity-locked position (powerless, identity_locked) — internalize the goal of nativity but cannot access it within the constraint
 *   - liturgical_hebrew_preservationists: excluded agenda-setters (powerful, constrained exit) — mount continuous resistance via textual authority
 *   - israeli_native_speakers: analytical observers who function as the implicit standard (powerful, arbitrage exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.62).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.48).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Contact Language for Diaspora Cohesion").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/identity_coordination").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '9df18382-cd51-42fc-a854-24e9f1558e2b').
narrative_ontology:cs_kernel_codification('9df18382-cd51-42fc-a854-24e9f1558e2b', distributed).
narrative_ontology:cs_authority_grounding('9df18382-cd51-42fc-a854-24e9f1558e2b', practice).
narrative_ontology:cs_interpretation_layer_present('9df18382-cd51-42fc-a854-24e9f1558e2b').
narrative_ontology:cs_reading_relation('9df18382-cd51-42fc-a854-24e9f1558e2b', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('9df18382-cd51-42fc-a854-24e9f1558e2b', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('9df18382-cd51-42fc-a854-24e9f1558e2b', foundational, pragmatic_communication_over_authenticity).
narrative_ontology:cs_axiom_status(pragmatic_communication_over_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('9df18382-cd51-42fc-a854-24e9f1558e2b', pragmatic_communication_over_authenticity, instrumental).
narrative_ontology:cs_axiom('9df18382-cd51-42fc-a854-24e9f1558e2b', secondary, standardization_as_legitimate_language_form).
narrative_ontology:cs_axiom_status(standardization_as_legitimate_language_form, holdable).
narrative_ontology:cs_axiom_grounding('9df18382-cd51-42fc-a854-24e9f1558e2b', standardization_as_legitimate_language_form, conventional).
narrative_ontology:cs_reference_frame('9df18382-cd51-42fc-a854-24e9f1558e2b', supra_regional_contact_language_standard).
narrative_ontology:cs_drift_state('9df18382-cd51-42fc-a854-24e9f1558e2b', contemporary_diaspora_assimilation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9df18382-cd51-42fc-a854-24e9f1558e2b', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, hebrew_educators).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, institutional_hebrew_authorities).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, heritage_language_learners).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, native_speaker_aspirants).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, linguistic_pragmatism_over_nativity).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, instrumental_identity_cohesion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use Hebrew (at varying fluency levels) to maintain intra-community communication across linguistic fragmentation: Yiddish speakers, Ladino speakers, Arabic-speaking Jews, and local-language-dominant Jews all convene in marketplace Hebrew. They set norms for acceptable register-mixing and define which vocabulary innovations 'count.' They benefit from cohesion; they also bear the cost that the language remains pidginized rather than deepening toward nativity or formal grammatical maturity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities, agenda_setter).

% Teach Hebrew as a second or heritage language in diaspora communities, Jewish day schools, and adult-education programs. They enforce the bridge-pidginized constraint by teaching a standardized, supra-regional register that is neither the Hebrew of native Israeli speakers nor the classical Hebrew of liturgy — a deliberate compromise. They benefit from stable institutional roles; they must actively defend the pidginized standard against pressure from native-speaker nativism and from liturgical purists.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_educators, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, hebrew_educators, beneficiary).

% Include the Hebrew Language Academy (Israel), Zionist education networks, and religious institutions that codify Hebrew norms. They set standardized curricula, dictionaries, and pedagogical materials that instantiate the bridge-pidginized reading: accepting loanwords, register-mixing, and instrumental use while resisting full nativization. They wield institutional authority to maintain this framing against both liturgical and nativist challenges.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, institutional_hebrew_authorities, agenda_setter,
    institutional, civilizational, mobile, global).

% Study Hebrew in diaspora educational settings to connect with ancestral heritage. They are told 'this is Hebrew' while learning a standardized, non-native variety that differs substantially from both classical liturgical Hebrew and contemporary native Israeli speech. They bear the cost that their learned Hebrew may not transfer to real-time interaction with native speakers or grant access to classical texts without further study.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, heritage_language_learners, payer,
    moderate, biographical, constrained, global).

% Diaspora speakers (often second-generation or heritage learners) who internalize the goal of native-speaker fluency and find themselves unable to achieve it within the pidginized constraint. They face persistent identity-boundary experiences: their Hebrew is corrected by native speakers, their grammar is treated as 'accented' or 'foreign,' their register choices are read as errors rather than acceptable variation. Exit (stopping Hebrew use) carries identity cost because Hebrew use has been framed as authentically Jewish.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_speaker_aspirants, payer,
    powerless, biographical, identity_locked, global).

% Argue that Hebrew's true continuity lives in preserved classical texts, liturgical recitation, and scholarly transmission — not in everyday marketplace interaction. They are structurally excluded from setting diaspora education policy but mount continuous resistance through religious authority, textual scholarship, and communal pressure. The bridge-pidginized reading dismisses their frame as 'not living language' and continues pedagogical standardization around instrumental utility instead of textual authority.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_hebrew_preservationists, excluded,
    powerful, civilizational, constrained, global).

% Represent the generative native-speaker norm against which diaspora Hebrew is measured and often found deficient. They are not direct stakeholders in diaspora community cohesion but function as the implicit standard by which diaspora speech is evaluated. Their native intuition is treated as the ground truth; diaspora variation is treated as departure from it, not as legitimate code-switching or contact-language innovation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, israeli_native_speakers, observer,
    powerful, biographical, arbitrage, national).

% The linguistic ecology that Hebrew is embedded in: English (in Anglophone diaspora), French (in Francophone diaspora), Russian (in post-Soviet diaspora), etc. The bridge-pidginized constraint exists partly by actively suppressing full code-switching into these languages and maintaining Hebrew as the in-group marker. The constraint's persistence depends on keeping Hebrew visible and necessary for community membership despite easier alternative communication channels.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, colonial_languages, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(hebrew_continuity__bridge_pidginized, colonial_languages).

% External researcher or meta-institutional observer studying the constraint's operation across multiple diaspora communities. Sees the structural dynamics: how the pidginized bridge-form persists despite both liturgical and nativist pressure, why heritage learners internalize identity-lock around 'authentic' Hebrew they cannot access, how institutional authority enforces register-mixing as the legitimate form.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, analytic_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, institutional_hebrew_authorities).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the linguistic fragmentation of diaspora Judaism: speakers of mutually unintelligible heritage languages (Yiddish, Ladino, Arabic, etc.) and native speakers of local majority languages require a common code for intra-communal communication. A standardized, supra-regional Hebrew fills this gap. The constraint coordinates community identity and institutional continuity across linguistic diversity.
% TRANSFER_FUNCTION: Transfers linguistic authority and definitional power from native-speaker intuition and classical-text scholarship to educators and institutional authorities. Also transfers authenticity-claims from textual mastery and native fluency to instrumental community participation. Native speakers lose status as the ground truth; liturgical scholars lose status as keepers of the 'real' Hebrew; diaspora educators gain institutional power to define what 'counts' as Hebrew.
% ABSENT_VOICES: Native Israeli speakers (excluded from diaspora policy-setting despite being treated as the implicit standard). Liturgical scholars (excluded because their textual-authority framing is incompatible with the instrumental-utility frame). Monolingual diaspora members who use only heritage languages or local languages (structurally invisible because they don't participate in Hebrew). Other minority languages within diaspora (Judeo-Persian, Judeo-Greek) that have atrophied further because resources have concentrated on standardized Hebrew.
% DISAPPEARANCE_RATIONALE: If the bridge-pidginized constraint disappeared, diaspora communities would face two pressures: either revert to heritage languages (Yiddish-medium Jewish culture resurges, internal fragmentation increases) or assimilate fully into local majority languages (Jewish identity cohesion weakens, institutional continuity degrades). The constraint enables diaspora Jewish institutional survival across linguistic diversity; without it, the ecosystem reorganizes around either stronger linguistic pluralism or faster assimilation.
% FOUNDING_PROBLEM: Post-enlightenment diaspora Jewish communities faced a linguistic crisis: traditional religious education had transmitted classical Hebrew and Yiddish, but as communities moved into Anglophone, Francophone, and other linguistic zones, a common second language was needed for community-internal coordination that worked across heritage-language divides.
% FOUNDING_PROBLEM_CORROBORATION: Hebrew educators and institutional authorities attest the problem was live and continues: diaspora communities remain linguistically fragmented and require a common code. Liturgical scholars attest the problem was never about living language but about preserving textual transmission. Native Israeli speakers and diaspora heritage learners attest the founding problem was partially solved but at the cost of a pidginized register that gates access to both native fluency and classical mastery. Independent sociolinguistic research on diaspora Hebrew confirms the problem persists and the constraint provides partial solution with structural tradeoffs.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the constraint converts linguistic diversity (which community members could navigate through multilingualism or local-language assimilation) into a single standardized form that is neither fully native nor fully classical. This conversion extracts authenticiy-claims and redirects them through institutional authority. Suppression is moderate (0.48) because the constraint is sustained partly by educational authority and partly by internalized identity-identification with Hebrew — it is not purely coercive (alternatives exist; heritage learners could refuse) but it is not freely chosen in the way a rope would be. Theater rises substantially (0.22 → 0.41 over the interval) because as the constraint persists, its functional coordination value plateaus while its maintenance costs grow: educators must continually defend the pidginized standard against liturgical and nativist pressure; institutional authorities must refresh curricula to keep the form stable; community members must increasingly perform their commitment to the form to maintain group membership. The measurement series on one shared time grid allows the engine to detect this drift: rising theater + plateauing extractiveness signals a constraint shifting from coordination toward performance. Accessibility collapse is moderate-high (0.71) because once the bridge-pidginized form is recognized as the community standard, alternatives collapse: heritage languages are framed as 'not community language,' native Hebrew is framed as 'outside diaspora context,' and local majority languages are framed as 'not Jewish.' Resistance is moderate (0.58) because the constraint meets real resistance from both liturgical and nativist seats, but that resistance is structurally dampened by institutional authority.
 *
 * PERSPECTIVAL GAP:
 *   Diaspora_jewish_communities and hebrew_educators should compute toward rope or even mountain (the coordination is genuine, the form is stable). Heritage_language_learners should compute toward tangled_rope or snare (they benefit from coordination but pay via non-transferable skills and identity-lock). Native_speaker_aspirants should compute toward snare (the constraint extracts their fluency aspirations). Liturgical_hebrew_preservationists should compute the constraint as snare (it suppresses classical authority and redirects resources). Israeli_native_speakers should compute it as piton or degraded (it performs nativism without delivering it). The engine computes these divergences from the structural data (power, exit, beneficiary/victim declarations) — the authored claim does NOT adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities are structural beneficiaries (they solve the coordination problem they face; they choose to participate; d low, near beneficiary end). Educators are beneficiaries-with-enforcement authority (they coordinate AND set the standard; they could change it; d moderate-beneficiary end). Heritage learners are near-symmetric but slightly toward target: they benefit from coordination but the form is non-negotiable (d ~0.5, slight upward bias). Native-speaker aspirants are targets (the form EXCLUDES them from their identity goal; identity-lock means they cannot exit; d high, near target end). Liturgical preservationists are excluded targets (they would object if present; the constraint suppresses their authority; d high, target end). The directionality derivation chain: beneficiary/victim declarations + exit_options → d. Native_speaker_aspirants are listed in victims[] and carry identity_locked exit; heritage_language_learners are listed in beneficiaries[] but carry constrained exit; the engine derives high d for victims + identity_lock, moderate d for beneficiaries + constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling through the tangled_rope classification: it possesses BOTH a genuine coordination function (solves diaspora linguistic fragmentation) AND asymmetric extraction (converts linguistic plurality into standardized form, redirects authenticity-claims through institutional authority, locks heritage learners and aspirants into identity-dependent positions). Without the tangled_rope gate, the constraint could be mis-classified as pure rope (the coordination is real, but the extraction is invisible) or mis-classified as piton (it is performative at the institutional level, but the coordination at the community level is live). The tangled_rope claim centers the structural reality: this is a coordination constraint that ALSO extracts through standardization and identity-locking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nativity_aspiration_internalization,
    'To what extent is the native-speaker aspiration a structural internalization of the constraint versus an autonomous identity goal that would exist regardless?',
    'Longitudinal studies tracking diaspora speakers who were NOT exposed to nativist framing: do they develop the aspiration spontaneously, or only when native-speaker norms are made visible through institutional comparison?',
    'If aspiration is internalized, the constraint''s extractiveness is higher than the authored suppression metric suggests — targets carry the suppression with them after learning stops. If aspiration is autonomous, the constraint is simply misaligned with pre-existing identity goals, not actively extracting them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nativity_aspiration_internalization, empirical, 'Whether native-speaker aspiration is structurally induced or pre-existing.').

omega_variable(
    coordination_necessity_counterfactual,
    'Is the bridge-pidginized standardized form NECESSARY for diaspora Jewish coordination, or would diaspora communities achieve comparable cohesion through code-switching, multilingualism, or reliance on liturgical Hebrew?',
    'Comparative study of diaspora communities that adopted different linguistic strategies (Hebrew standardization vs. maintained multilingualism vs. liturgical-only transmission) and measurement of institutional continuity, intergenerational language transmission, and community satisfaction metrics.',
    'If the standardized form is necessary, the constraint is a genuine rope with extraction costs that are unavoidable. If alternatives achieve comparable coordination, the standardization is an institutional choice that imposes unnecessary extraction on heritage learners and aspirants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_counterfactual, empirical, 'Whether the constraint represents the unique coordination solution or an institutional preference.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the bridge-pidginized reading logically foreclose the native-generative reading within any single institutional framework, or do they remain live alternatives?',
    'Examine whether institutions claiming to promote the bridge-pidginized form have ALSO funded native-speaker acquisition programs (Israeli immersion, native-speaker exchange, etc.), indicating coexistence; or whether they have actively suppressed native-speaker pathways, indicating foreclosure.',
    'If foreclosure is real, the constraint is more extractive than the authored metrics suggest — it actively suppresses an alternative epistemic frame. If coexistence is real, the constraint is more of a default-setting than a suppressant — heritage learners could access native pathways if institutional priority shifted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether this reading structurally precludes the native-generative reading or whether both remain institutional options.').

omega_variable(
    institutional_authority_source,
    'From what sources do educators and authorities derive their definitional power over the bridge-pidginized standard? Religious authority? Linguistic science? Pragmatic community pressure? Market demand for Hebrew literacy?',
    'Historical-institutional analysis: trace the founding authority claims for diaspora Hebrew curricula; examine which actors gained/lost institutional power as standardization took hold.',
    'If authority is derived from community consensus, the constraint''s extractiveness is lower (communities chose this form). If authority is imposed by external institutions or colonial-legacy authorities, the constraint''s suppression is higher (extraction is coercive, not consensual).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_authority_source, empirical, 'The institutional genealogy of the constraint''s enforcement authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t12, hebrew_continuity__bridge_pidginized, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(hebr_tr_t12, observed).
narrative_ontology:measurement(hebr_tr_t25, hebrew_continuity__bridge_pidginized, theater_ratio, 25, 0.32).
narrative_ontology:measurement_basis(hebr_tr_t25, observed).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__bridge_pidginized, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(hebr_tr_t50, observed).
narrative_ontology:measurement(hebr_tr_t75, hebrew_continuity__bridge_pidginized, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(hebr_tr_t75, observed).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__bridge_pidginized, theater_ratio, 100, 0.41).
narrative_ontology:measurement_basis(hebr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t12, hebrew_continuity__bridge_pidginized, base_extractiveness, 12, 0.51).
narrative_ontology:measurement_basis(hebr_be_t12, observed).
narrative_ontology:measurement(hebr_be_t25, hebrew_continuity__bridge_pidginized, base_extractiveness, 25, 0.56).
narrative_ontology:measurement_basis(hebr_be_t25, observed).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__bridge_pidginized, base_extractiveness, 50, 0.61).
narrative_ontology:measurement_basis(hebr_be_t50, observed).
narrative_ontology:measurement(hebr_be_t75, hebrew_continuity__bridge_pidginized, base_extractiveness, 75, 0.62).
narrative_ontology:measurement_basis(hebr_be_t75, observed).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__bridge_pidginized, base_extractiveness, 100, 0.62).
narrative_ontology:measurement_basis(hebr_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t12, hebrew_continuity__bridge_pidginized, suppression_requirement, 12, 0.38).
narrative_ontology:measurement_basis(hebr_su_t12, observed).
narrative_ontology:measurement(hebr_su_t25, hebrew_continuity__bridge_pidginized, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(hebr_su_t25, observed).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__bridge_pidginized, suppression_requirement, 50, 0.47).
narrative_ontology:measurement_basis(hebr_su_t50, observed).
narrative_ontology:measurement(hebr_su_t75, hebrew_continuity__bridge_pidginized, suppression_requirement, 75, 0.48).
narrative_ontology:measurement_basis(hebr_su_t75, observed).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__bridge_pidginized, suppression_requirement, 100, 0.48).
narrative_ontology:measurement_basis(hebr_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, diaspora_jewish_institutional_continuity).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, yiddish_language_atrophy).

% DUAL FORMULATION NOTE:
% Part of the hebrew_continuity kernel family. This story (bridge_pidginized) represents the pragmatic, instrumental reading. The liturgical_preservation and native_generative stories are sibling readings of the same kernel; each has a different ε, different beneficiary/victim structure, and different classification. The three stories together model the contested definition of 'Hebrew continuity' across diaspora contexts. Decomposition principle: the three readings cannot coexist in a single story because their ε-referents differ (what counts as 'continuity' is the contested claim), and they instantiate different structural constraints on who benefits and who pays. Each story gets its own clean ε, independent beneficiary/victim declarations, and structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__bridge_pidginized, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
