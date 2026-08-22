% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Vernacularization of the Holy Tongue as Assessed by the Liturgical-Preservation Reading
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'what makes
 *   Hebrew linguistically alive': the liturgical_preservation_reading, which
 *   holds that a language lives in the continuous recitation, study, and
 *   transmission of its sacred texts regardless of vernacular use. On this
 *   reading Hebrew never died — the chain of transmission never broke — and
 *   Ben-Yehuda's project was therefore not resurrection but desecration: the
 *   consumption of a sanctified register for mundane national purposes. The
 *   epsilon referent is the standing arrangement under contest — the
 *   vernacularization of the holy tongue by the Zionist and Israeli state
 *   apparatus — assessed strictly by this reading's own lights; the reading's
 *   endorsed alternative (the liturgical-preservation regime) is NOT the
 *   referent and contributes nothing to epsilon. The victim set is
 *   deliberately non-standard: the sacred tradition itself, carried here by
 *   the human transmission chain (agent-bearing, feeds derivation) and by the
 *   sanctity of the tongue (declared non-agentive, narrative completeness
 *   only). Claim and metrics are independent authored facts: the claimed type
 *   is tangled_rope because the arrangement undeniably solves a real
 *   coordination problem while extracting gravely through the same structure;
 *   the metrics describe that operation as this reading assesses it. Sibling
 *   readings are separate files linked through the network, not folded into
 *   this one.
 *
 * KEY AGENTS:
 *   - - sacred_transmission_chain: Primary target (organized/identity_locked) — bears the desecration of its charge and the loss of linguistic authority; its unbroken practice is the very fact this reading treats as proof of life
 *   - - lashon_hakodesh_sanctity: Declared victim per this reading (non-agent, excluded from derivation) — the sanctified status of the tongue, consumed without consent
 *   - - secular_zionist_institutions: Agenda-setter and primary beneficiary (institutional/arbitrage) — built, enforced, and narratively owns the vernacular arrangement; collects its returns
 *   - - secular_hebrew_literary_class: Secondary beneficiary (moderate/mobile) — consumes the sacred register as aesthetic raw material without transmission obligations
 *   - - israeli_hebrew_speaking_public: Dual-positioned (moderate/constrained) — receives coordination benefits while unknowingly performing the consumption the tradition forbids
 *   - - comparative_linguistics_observers: Analytical observer — documents continuity, borrowing, and suppression history without taking a sanctity position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.76).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.52).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Vernacularization of the Holy Tongue as Assessed by the Liturgical-Preservation Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '136d8f70-ce1f-4ec3-8d27-843764f03f68').
narrative_ontology:cs_kernel_codification('136d8f70-ce1f-4ec3-8d27-843764f03f68', distributed).
narrative_ontology:cs_authority_grounding('136d8f70-ce1f-4ec3-8d27-843764f03f68', lineage).
narrative_ontology:cs_interpretation_layer_present('136d8f70-ce1f-4ec3-8d27-843764f03f68').
narrative_ontology:cs_reading_relation('136d8f70-ce1f-4ec3-8d27-843764f03f68', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('136d8f70-ce1f-4ec3-8d27-843764f03f68', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('136d8f70-ce1f-4ec3-8d27-843764f03f68', foundational, sanctity_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(sanctity_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('136d8f70-ce1f-4ec3-8d27-843764f03f68', sanctity_constitutes_linguistic_life, theological).
narrative_ontology:cs_axiom('136d8f70-ce1f-4ec3-8d27-843764f03f68', foundational, vernacularization_desecrates_sacred_register).
narrative_ontology:cs_axiom_status(vernacularization_desecrates_sacred_register, holdable).
narrative_ontology:cs_axiom_grounding('136d8f70-ce1f-4ec3-8d27-843764f03f68', vernacularization_desecrates_sacred_register, theological).
narrative_ontology:cs_reference_frame('136d8f70-ce1f-4ec3-8d27-843764f03f68', unbroken_liturgical_transmission_continuum).
narrative_ontology:cs_drift_state('136d8f70-ce1f-4ec3-8d27-843764f03f68', post_vernacularization_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('136d8f70-ce1f-4ec3-8d27-843764f03f68', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, secular_zionist_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_literary_class).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_transmission_chain).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, lashon_hakodesh_sanctity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, israeli_hebrew_speaking_public).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, israeli_hebrew_speaking_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The multi-generational community of rabbinic scholars, cantors, and lay reciters who have maintained daily recitation, study, and transmission of the sacred corpus — Tanakh, Mishnah, Talmud, siddur — in Hebrew and Aramaic without interruption, including the living traditionalist communities that continue this practice today. Under the vernacular arrangement their charge is consumed: the tongue they guard circulates as ordinary speech, norm-setting authority passes to state academies, their pronunciation variant is stigmatized as archaic, and their account of the language is recast as pious folklore. Leaving the chain would mean abandoning the transmission obligation that constitutes their identity, so exit is not a realistic option; they bear the cost while continuing to perform the very activity the arrangement renders invisible.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_transmission_chain, payer,
    organized, civilizational, identity_locked, global).

% The sanctified status of the Hebrew tongue as accumulated across the transmission chain — a non-agentive good carried by the tradition rather than held by any actor. Under the vernacular arrangement it is consumed without consent or compensation: each mundane deployment spends the register's covenantal charge, and no mechanism exists for it to refuse, bargain, or withdraw. Listed for narrative completeness because this reading locates the primary injury in the tradition itself; excluded from directional computation as a non-agent.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, lashon_hakodesh_sanctity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, lashon_hakodesh_sanctity).

% The yishuv bodies, the Hebrew Language Committee and its successor the Academy of the Hebrew Language, the school systems, and after 1948 the state ministries and IDF education infrastructure that made Hebrew the spoken public language. They standardized grammar, orthography, and pronunciation, compelled acquisition through schooling and military service, adjudicated the Language Wars against Yiddish and German, and propagated the 'revival of a dead language' narrative that frames the traditionalist account as myth. They set the norms the language follows and collect the national-legitimacy and administrative returns of having done so.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Poets, novelists, journalists, and academics who draw on the inherited sacred register — biblical idiom, rabbinic allusion, liturgical cadence — as raw material for secular art and scholarship. They consume accumulated stylistic and sanctity-bearing capital without carrying transmission obligations, and their mobility between languages, genres, and markets means the arrangement costs them little they could not replace.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, secular_hebrew_literary_class, beneficiary,
    moderate, biographical, mobile, national).

% Ordinary citizens who speak revived Hebrew as their daily vernacular, educated into it by state schools and the army. They receive the coordination benefits of a shared language — commerce, administration, civic participation — and, in this reading's accounting, carry the desecration cost unknowingly, since their mundane speech is precisely the consumption the tradition forbids. Exit would mean emigration or self-segregation into traditionalist enclaves; neither is realistically available to most.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, israeli_hebrew_speaking_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, israeli_hebrew_speaking_public, payer).

% Linguists and historians who document the empirical substrate beneath the dispute: the continuous vitality of rabbinic Hebrew as a written and scholarly register, the degree to which the revived vernacular borrowed from rather than reconstructed the liturgical language, and the actual course of the yishuv's language politics. They take no position on sanctity, but their findings discipline the factual claims of every reading of the kernel.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, comparative_linguistics_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, secular_zionist_institutions).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the collective-action problem of a polyglot returning population — speakers of Yiddish, Ladino, Judeo-Arabic, Russian, Polish, and German with no common vernacular — by supplying one shared medium for commerce, administration, education, and military command, and gave a dispersed nation a single linguistic signifier of sovereignty.
% TRANSFER_FUNCTION: Moves linguistic authority and sacred capital: transfers the holy tongue and its interpretive tradition out of the transmission chain's custody into general secular circulation, and transfers norm-setting power from rabbinic custodians to state academies and the secular literary class.
% ABSENT_VOICES: The traditionalist transmitters objected in real time — leading rabbinic authorities of the late nineteenth and early twentieth centuries condemned vernacularization as desecration — but their objections were structurally excluded from the yishuv's language councils, the Mandate-era committees, and later the Academy, where the arrangement's architects sat in judgment on their own project. The sacred tradition itself, being non-agentive, had no seat anywhere and no procedural way to withhold what was taken.
% DISAPPEARANCE_RATIONALE: If the vernacular arrangement vanished overnight, Israeli public life would rearrange immediately: administration, courts, universities, and the army would lose their shared medium, millions would fall back on heterogeneous heritage languages, and the state's symbolic foundation — the equation of Hebrew speech with national belonging — would collapse. Even the traditionalist world, now entangled with the state through subsidies, employment, and shared currency, would face disruptive adjustment. Whatever this reading thinks of the arrangement's legitimacy, the world is built on it.
% FOUNDING_PROBLEM: The polyglot condition of the Jewish return to the Land: immigrant communities from dozens of language backgrounds with no common vernacular, confronting a nationalist movement that wanted a unifying indigenous language to anchor political sovereignty and cultural continuity.
% FOUNDING_PROBLEM_CORROBORATION: The arrangement's beneficiaries attest the problem is still live, citing each new immigration wave's integration needs. Outside the benefiting parties, sociolinguistic histories of the yishuv corroborate that the coordination problem was real but that functioning alternative media (Yiddish, German, Arabic) existed, and traditionalist rabbinic sources outside the Zionist apparatus attest that the sacred register required no vernacularization to remain fully alive. On the specific claim that the holy tongue itself needed secular speech, no corroborating voice outside the beneficiary set exists — the record is silent or hostile.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.76 at interval end) because this reading weighs the consumed good — an irreplaceable sanctified register — as beyond price, and the consumption was performed without the custodians' consent and against their explicit protest. Suppression (0.52) traces an enforcement arc rather than a plateau: informal campaigning in the 1880s, open warfare against Yiddish and German in the Mandate era (the Language Wars), compulsory state acquisition machinery after 1948, then consolidation and partial decay as hegemony made active coercion redundant — which is why suppression_requirement is tracked on the shared grid despite the static-picture scalar rule. Theater (0.36) captures the 'miracle of revival' pageantry — Ben-Yehuda commemoration, anniversary ceremonies, heritage exhibits — layered over a genuinely functional daily language; the function is real, the resurrection narrative is partly performance. Accessibility_collapse (0.55): alternatives collapsed thoroughly inside the yishuv and later the state, but survived intact outside it — the Haredi world retained Yiddish vernacular and its own pronunciation, so exit remained visible even where it was costly. Resistance (0.65): sustained and ongoing — rabbinic condemnations, retention of Ashkenazi pronunciation in Torah study to the present, curricular secession, refusal of secular Hebrew literature in parts of the traditionalist world. All three metric series run on one shared time grid (1881, 1922, 1948, 1970, 2000, 2025) so no row substitutes an end-state scalar for an earlier value.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the agenda-setter seat the arrangement is a triumphant coordination achievement it built and legitimately administers — low directionality, extraction damped toward subsidy. From the transmission-chain seat the same structure operates as grave extraction amplified by identity lock: the target cannot exit without dissolving the identity the constraint constitutes, pushing effective extraction toward the full-target end. The speaking public sits split — beneficiary of the coordination, unwitting payer of the desecration — and should compute intermediate. The observer seat sees structure without stakes. The engine derives these divergences from the declared data; this story's claimed type adjudicates nothing between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to the low-d end: secular_zionist_institutions (agenda-setter, collects the returns, arbitrage-grade exit) and secular_hebrew_literary_class (mobile consumers of the register). Victims map to the high-d end: sacred_transmission_chain (bears the transfer, identity_locked, organized) and, declaratively per this reading, lashon_hakodesh_sanctity (non-agentive, excluded from the arithmetic by design). One override is declared: the derivation from the chain's dual exposure — it incidentally receives state funding and its members gain employability in the revived-language economy — would drag its derived d toward symmetry, understating its true position; the override sets the organized atom to 0.75 to reflect that its primary structural relationship is target, with incidental benefits masking rather than offsetting the extraction. Scope is national-dominant with a global traditionalist diaspora, so scope amplification is modest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-communal coordination for a polyglot population — was substantially solved decades ago, yet the arrangement's machinery has expanded rather than sunset: an active Academy regulating pronunciation and coinage, ongoing enforcement of Hebrew-only public spheres, and a commemorative industry sustaining the revival narrative. The R5 interview records this as a contested status over a world_rearranges verdict, which is exactly the mismatch profile that flags zombie tendency: the arrangement persists past its justification, defended now by inertia and narrative rather than by the original need. The tangled_rope classification prevents mislabeling in both directions: it is not a rope, because the extraction is grave and has identifiable victims this reading refuses to discount; it is not a snare, because the coordination function is genuine and conceded even from this hostile seat — the objection is to what was consumed and on whose authority, not to whether coordination occurred. Nor is it a piton: the gains concentrate in a named seat, so the receipt surface records capture, and fixing is prohibitive because a spoken language cannot be unspeakn.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_routing,
    'This constraint is one reading (liturgical_preservation_reading) of the kernel hebrew_linguistic_life; what would the sibling readings (native_generational_reading, marketplace_pidgin_reading) change structurally if instantiated instead?',
    'Comparative classification across the three reading-stories of the kernel: each sibling carries its own epsilon, beneficiary/victim sets, and claimed type, and cross-reading divergence is read from the compiled corpus rather than negotiated inside any one story.',
    'Under native_generational_reading the victim set becomes children denied mother-tongue acquisition and the arrangement under assessment inverts; under marketplace_pidgin_reading the victim set becomes communities lacking a practical inter-communal medium. Epsilon re-bins accordingly; this story''s high extraction is a property of this reading''s valuation, not of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_routing, conceptual, 'Committer-frame routing: which reading of the linguistic-life kernel this story instantiates and what siblings would alter.').

omega_variable(
    non_agent_victim_derivation,
    'Can the declared victim — the sacred tradition itself — bear extraction, or must the costs of desecration resolve entirely onto the human carriers of the transmission chain?',
    'Compare the compiled directionality for the agent-bearing chain seat against the narrative declaration of the non-agentive sanctity seat; if the engine''s computed extraction tracks only the human carriers, the reading''s victim-set claim is carried by proxy rather than directly.',
    'If costs resolve only onto human carriers, effective extraction is bounded by the chain''s organized power and identity lock; if the tradition itself is admitted as a bearer, the reading''s severity claim stands unconstrained by any actor''s bargaining position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_agent_victim_derivation, conceptual, 'Whether a non-agentive tradition can sit in the victim set or only its human transmitters can.').

omega_variable(
    desecration_measurability,
    'Does loss of sanctity admit any measure that is not simply the liturgical_preservation_reading''s own valuation restated?',
    'Search for reading-neutral proxies: measurable displacement of norm-setting authority from rabbinic to state institutions, quantifiable share of the revived lexicon drawn from the liturgical register, documented suppression events. Where proxies exist, anchor epsilon to them; where none exist, accept that epsilon is irreducibly reading-indexed.',
    'If no neutral proxy exists, the story''s high extraction is a faithful report of this reading''s lights and cannot be cross-validated against rival readings — classification divergence between the sibling stories becomes the only available signal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(desecration_measurability, empirical, 'Whether sanctity-loss is measurable or irreducibly reading-indexed.').

omega_variable(
    resacralization_possibility,
    'Is re-sacralization of the vernacularized tongue structurally possible — could the holy register be withdrawn back into sacral custody after more than a century of mundane saturation?',
    'Historical comparison with registers that were successfully re-sacralized or abandoned after vernacularization, and analysis of whether any mechanism exists for a modern state''s majority language to be ritually restricted.',
    'If re-sacralization is impossible, the extraction recorded here is irreversible and the arrangement''s cost profile is permanently elevated; if possible in principle, the prohibitive fixing-cost is contingent on politics rather than structure, and long-run drift toward inertial maintenance becomes conceivable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resacralization_possibility, empirical, 'Whether the desecration the reading alleges is reversible, governing the permanence of its cost claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 1881, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1881, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1881, 0.08).
narrative_ontology:measurement(hebr_tr_t1922, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1922, 0.16).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1948, 0.26).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1970, 0.32).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(hebr_tr_t2025, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2025, 0.36).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1881, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1881, 0.28).
narrative_ontology:measurement(hebr_be_t1922, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1922, 0.45).
narrative_ontology:measurement(hebr_be_t1948, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1948, 0.62).
narrative_ontology:measurement(hebr_be_t1970, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1970, 0.71).
narrative_ontology:measurement(hebr_be_t2000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement(hebr_be_t2025, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2025, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1881, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1881, 0.22).
narrative_ontology:measurement(hebr_su_t1922, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1922, 0.48).
narrative_ontology:measurement(hebr_su_t1948, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(hebr_su_t1970, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(hebr_su_t2000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(hebr_su_t2025, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'Is Hebrew alive?' decomposes into three structurally distinct claims, one per reading of the kernel hebrew_linguistic_life. Each member carries its own epsilon, victim set, and claimed type; this member (liturgical_preservation_reading) is the historically upstream formulation — the pre-revival consensus criterion — whose framework the two downstream siblings were formulated against, which is why its edges point at both. Epsilon differs across the family because the referent arrangement and the weighing of the consumed good differ per reading, not because any member measures the same constraint with a different observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
