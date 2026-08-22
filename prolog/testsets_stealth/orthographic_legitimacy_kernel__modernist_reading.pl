% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Modernist Orthographic Legitimacy Reading — Civilizational Rupture Script Regime
 *   domain: political linguistics / state formation / commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   orthographic legitimacy — what makes a way of writing the national
 *   language authoritative. The modernist_reading holds that legitimacy flows
 *   from alignment with Western/European modernity and rupture from the
 *   Ottoman/Islamic past; it is the reading under which the 1928-style
 *   alphabet revolution was enacted: a statute mandating the Latin-based
 *   script, criminalizing the old letters in public and official use, and
 *   building a schooling apparatus through which the entire population is
 *   re-literated. Under this reading the destruction of the old literate
 *   sphere is not a side effect but the constitutive achievement — the point
 *   of the exercise. The claim and the metrics are independent authored
 *   facts: claimed_type is tangled_rope because the structure carries a
 *   genuine coordination function (standardized phonetic script, mass
 *   literacy, print integration) AND asymmetric extraction (a whole class's
 *   cultural capital voided by fiat) held together by active enforcement. The
 *   sibling readings — continuity_reading and instrumentalist_reading — are
 *   separate constraints with their own epsilon and victim structures and are
 *   linked only through the network edges; nothing about them is averaged
 *   into this file.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: Agenda-setter (institutional/arbitrage) — writes, enforces, and collects from the script regime
 *   - ottoman_literate_class: Primary target (organized/identity_locked) — cultural capital voided by statute
 *   - religious_scholars_ulema: Primary target (organized/identity_locked) — transmission chains severed from successors
 *   - new_script_literate_generation: Secondary beneficiary (moderate/constrained) — credentials valuable only while the old corpus stays sealed
 *   - adult_peasants_and_conscripts: Nominal beneficiary, effectively near-symmetric bearer (powerless/trapped) — compelled retraining and examination fines against thin gains
 *   - ottoman_script_press: Excluded actor (organized/trapped) — its exclusion is the enforcement object
 *   - calligraphers_and_manuscript_trades: Collateral target (moderate/trapped) — craft market abolished
 *   - modernization_historians: Analytical observer — measures attribution and archives the displaced testimony
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.75).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.62).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Modernist Orthographic Legitimacy Reading — Civilizational Rupture Script Regime").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political linguistics / state formation / commitment systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, 'e06a32cb-05f5-48e3-b362-8153ddb0975c').
narrative_ontology:cs_kernel_codification('e06a32cb-05f5-48e3-b362-8153ddb0975c', formalized).
narrative_ontology:cs_authority_grounding('e06a32cb-05f5-48e3-b362-8153ddb0975c', extraction).
narrative_ontology:cs_interpretation_layer_present('e06a32cb-05f5-48e3-b362-8153ddb0975c').
narrative_ontology:cs_reading_relation('e06a32cb-05f5-48e3-b362-8153ddb0975c', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e06a32cb-05f5-48e3-b362-8153ddb0975c', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_axiom('e06a32cb-05f5-48e3-b362-8153ddb0975c', foundational, rupture_is_constitutive_of_national_rebirth).
narrative_ontology:cs_axiom_status(rupture_is_constitutive_of_national_rebirth, holdable).
narrative_ontology:cs_axiom_grounding('e06a32cb-05f5-48e3-b362-8153ddb0975c', rupture_is_constitutive_of_national_rebirth, deontological).
narrative_ontology:cs_axiom('e06a32cb-05f5-48e3-b362-8153ddb0975c', secondary, ottoman_script_embodies_clerical_backwardness).
narrative_ontology:cs_axiom_status(ottoman_script_embodies_clerical_backwardness, holdable).
narrative_ontology:cs_axiom_grounding('e06a32cb-05f5-48e3-b362-8153ddb0975c', ottoman_script_embodies_clerical_backwardness, empirically_contingent).
narrative_ontology:cs_reference_frame('e06a32cb-05f5-48e3-b362-8153ddb0975c', alignment_with_european_modernity).
narrative_ontology:cs_drift_state('e06a32cb-05f5-48e3-b362-8153ddb0975c', contemporary_neo_ottoman_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e06a32cb-05f5-48e3-b362-8153ddb0975c', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, new_script_literate_generation).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, calligraphers_and_manuscript_trades).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, adult_peasants_and_conscripts).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, adult_peasants_and_conscripts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates and administers the alphabet law: funds the national schools and people's houses that teach the new script, staffs the bureaucracy, courts, and army exclusively in it, and penalizes official use of the old letters. Gains a unified information space in which every path to literacy, employment, and the state itself runs through institutions it controls. Its position is strong: it wrote the rule and can amend, extend, or selectively waive it.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Educated entirely in the Latin-based orthography after the reform. Holds the credentials the state rewards: teaching posts, clerkships, newspaper work, officer commissions. Their advantage lasts as long as the old corpus stays sealed; reopening the archives to general readership would dilute the scarcity value of their formation.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, new_script_literate_generation, beneficiary,
    moderate, biographical, constrained, national).

% Poets, journalists, jurists, and senior clerks formed in the Arabic-letter school system. Within months of the law their reading and writing no longer qualify them for any public writing; their libraries remain legible only to themselves and a shrinking circle. Leaving the position means re-enrolling, late in life, as beginners in the alphabet of the regime that displaced them — a step their standing, self-concept, and livelihood all resist.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    organized, biographical, identity_locked, national).

% Preach, teach, and adjudicate through unbroken chains of Arabic-letter transmission — sermons, commentaries, legal compendia. The ban cuts them off from successors schooled only in the new script except through state-licensed channels; each cohort trained solely in Latin letters narrows their audience further. Their authority rests on a continuity they can no longer publicly perform, and converting to the new orthography would dissolve the very office they hold.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars_ulema, payer,
    organized, generational, identity_locked, national).

% Copyists, gilders, and master calligraphers whose market was official documents, liturgical manuscripts, and display inscriptions in the old script. Government offices stop commissioning; signboards come down; the trade contracts to private devotion and marginal commemorative work. Their skill has no resale in the new print economy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, calligraphers_and_manuscript_trades, payer,
    moderate, biographical, trapped, local).

% Newspapers, journals, and publishing houses built around Arabic-letter composition. Barred from setting type in the old script, they lose their readership contract overnight; serving that readership again would require the law itself to change. Their exclusion is what the enforcement machinery maintains.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_script_press, excluded,
    organized, biographical, trapped, national).

% Enrolled by decree in evening literacy courses and liable to fines for failing the new-script examinations. They acquire working literacy usable for state paperwork and cheap print, and they give up harvest labor, course attendance, and examination fees in return; the balance differs village by village and almost none of them were asked.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, adult_peasants_and_conscripts, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(orthographic_legitimacy_kernel__modernist_reading, adult_peasants_and_conscripts, payer).

% Later scholars and economists who measure the reform's literacy yields, compare modernizing states that changed script with those that did not, and archive the testimony of the displaced. They hold no stake in the statute and can read both bodies of sources.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernization_historians, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__modernist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single standardized phonetic orthography compatible with European printing, typewriting, and telegraphy, enabling uniform administration, mass literacy campaigns, and integration of the national press into European print distribution — problems previously handled through a script poorly suited to the language's vowel system and a fragmented manuscript culture.
% TRANSFER_FUNCTION: Moves cultural authority and textual access from the Ottoman-Islamic literate class to the republican state and its new-script intelligentsia; moves the entire written inheritance out of general reach; moves the population's symbolic orientation from the Islamic textual world toward Europe.
% ABSENT_VOICES: The ulema and the Ottoman literati were not seated in the alphabet commission whose output dispossessed them; parliamentary debate was brief and the law passed before those whose lives it restructured could organize a response. Ordinary adults subject to the literacy examinations were consulted by proclamation, not inquiry. The barred old-script press would have argued for dual-script publication and was kept out by the very rule at issue.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, schooling, bureaucracy, courts, press, and eventually digital infrastructure — all built on the new script — would lose their common medium; the literate population would be stranded between alphabets, and access to the sealed Ottoman corpus would reopen to whoever still read it, dissolving the state's information monopoly and the new literate class's scarcity advantage. Every major institution would have to renegotiate its medium.
% FOUNDING_PROBLEM: The young republic needed to break decisively with the Ottoman-Islamic order, make the revolutionary settlement irreversible, and reorient the nation toward Europe; the script was selected as the lever because literacy is administered by the state and the break is performed visibly by every citizen every day.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary foreign diplomats and journalists recorded the government's own stated aim of civilizational rupture, independent of the benefiting parties; later economic historians outside the state tradition attest both the scale of the literacy campaign and the dispute over its attribution; the memoirs and petitions of the displaced ulema corroborate the rupture's reality from the losing side. Corroboration of the genealogy therefore exists across hostile seats — what remains disputed is whether the founding problem was ever real or was manufactured to license the transfer.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.75 at interval end) because the transfer is deliberate and compounding: the reading's own logic requires dispossessing the old literate class, and each new cohort inherits the seal on the corpus, so the burden does not retire with the direct victims. Suppression (0.62) is a raw structural property — legal bans, licensed-only religious instruction, examination penalties — and is NOT scaled by power or scope in this field; the engine scales only extractiveness. The suppression_requirement series falls gently (0.88 to 0.62) as enforcement habituates, but never approaches zero because periodic campaigns against old-letter signage and unauthorized instruction recur. Theater_ratio rises (0.12 to 0.35) as the original teaching function completes and commemorative activity — anniversary rites, founder-at-the-chalkboard iconography, loyalty examinations — takes a growing share of the arrangement's visible operation. Accessibility_collapse (0.6): alternatives narrowed sharply but never to zero — clandestine religious instruction, diaspora publishing, and private devotion persisted, so this is not a natural-law profile. Resistance (0.55): parliamentary dissent, passive refusal by the old class, religious opposition, and later revival movements met the reform continuously. All three tracked series run on one shared time grid (t=0,5,10,15,20,25,30) so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the agenda-setter seat the arrangement is a nation-building instrument it designed and can revise — coordination with a civilizational purpose. From the ulema and Ottoman literati seats the same statute operates as expropriation enforced by criminal penalty, and their exit options are identity_locked in a specific sense: the binding is simultaneously professional (careers constituted in the old script), relational (authority transmitted through unbroken chains that the new script interrupts), ideological (a worldview in which the old letters carry revelation), and institutional (the office itself dissolves if its holder converts). Were that identity frame to break — a scholar deciding the tradition can survive translation — the seat's computed extraction would fall sharply. The peasant seat computes neither story cleanly: compelled schooling with fines, thin and unevenly distributed gains. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries and victims are declared in base_properties and mirrored on the stakeholder surface. The state apparatus sits near the beneficiary pole (declared beneficiary, arbitrage-grade exit — it authored the rule). The new-script generation is a constrained beneficiary. The ulema and Ottoman literate class sit near the full-target end: declared victims whose identity_locked exit removes the damping that mobility would provide. Calligraphers are trapped targets. One explicit override is authored: the derivation chain reads adult_peasants_and_conscripts as declared beneficiaries, which would push their d toward the subsidized end, but their actual position is near-symmetric (d = 0.55) — examination fines, forfeited labor, and compulsory course attendance offset thin, unevenly distributed literacy gains, and they were never consulted. The override corrects the derivation where the declared role misleads.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite misreadings. Read as pure coordination (rope), the reform's deliberate destruction of a literate class disappears into a literacy-success story; read as pure extraction (snare), the real delivered function — a phonetic script suited to the language, functioning mass schooling, print integration — vanishes into an elite-persecution story. Tangled_rope holds both: genuine coordination AND asymmetric extraction through the same structure, requiring active enforcement. On the genealogy interview, the founding problem (civilizational rupture) is arguably accomplished as originally posed, yet the arrangement persists and its commemorative apparatus grows — the status=contested x verdict=world_rearranges combination flags precisely this zone where a completed mission and a living structure must be explained by something other than the mission. Whether that residual is inertial maintenance or ongoing compounding extraction is what the temporal series and the irreversibility omega are positioned to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexing,
    'This constraint is one reading (modernist_reading) of the orthographic_legitimacy_kernel; what would the sibling readings (continuity_reading, instrumentalist_reading) change structurally if instantiated instead?',
    'Compare against the compiled sibling stories: the continuity reading assesses the same statute as pure destruction of traditional access (higher epsilon, snare-leaning victim structure); the instrumentalist reading keys epsilon to measured literacy and administrative-efficiency outcomes rather than to the fate of the old elite.',
    'Under the continuity reading the arrangement likely computes as a snare (no compensating coordination for the victim seat); under the instrumentalist reading it may compute closer to a rope with transition costs. The modernist reading''s distinctive move — treating dispossession of the old literate class as constitutive rather than incidental — is what holds epsilon high while preserving the coordination gate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexing, conceptual, 'Committer-frame indexing: which kernel reading this story instantiates and what siblings would alter.').

omega_variable(
    rupture_necessity_contest,
    'Was civilizational rupture via script replacement causally necessary for successful modernization, or was it one path among several available to the state?',
    'Comparative analysis of modernizing states that replaced their script versus those that modernized without rupture (e.g., states retaining Arabic, Chinese-character, or kana-based systems), controlling for schooling investment and economic policy.',
    'If rupture was unnecessary, the costs imposed on the traditional literate class were gratuitous and the arrangement leans toward pure extraction; if rupture was a genuine precondition as the modernist reading claims, part of the measured burden is the price of the transformation itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_necessity_contest, empirical, 'Whether the rupture the reading celebrates was structurally required or chosen.').

omega_variable(
    literacy_gain_attribution,
    'How much of the post-reform rise in literacy is attributable to the script change itself versus expanded schooling, conscription, and economic development?',
    'Econometric decomposition of literacy time series against schooling expenditure and enrollment data; comparison with non-reforming linguistically comparable states over the same period.',
    'If schooling investment dominates, the coordination-function warrant of the modernist reading thins and the arrangement''s persistence looks increasingly maintained by enforcement and identity politics rather than by delivered function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gain_attribution, empirical, 'Attribution of the literacy gains the reform claims as its product.').

omega_variable(
    generational_amnesia_irreversibility,
    'Is the severance of the general population from the Ottoman-script corpus practically irreversible, or is it revivable through digitization, specialist training pipelines, and curriculum change?',
    'Track Ottoman-script acquisition rates outside compulsory schooling, the reach of digitized manuscript collections, and curriculum reforms reintroducing old-script instruction.',
    'If revivable, the arrangement resembles a completed transition whose supporting coercion is obsolete (scaffold-like residue); if irreversible, the seal on the corpus is a permanent structural feature and the extraction compounds with every cohort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_amnesia_irreversibility, empirical, 'Whether the textual rupture can be undone or has become a fixed condition.').

omega_variable(
    old_script_stigma_internalization,
    'Is the continued absence of old-script literacy among the population maintained by legal restriction alone, or by internalized stigma attached to the old letters as markers of backwardness?',
    'Post-liberalization uptake studies: wherever restrictions on old-script publication and instruction relaxed, measure whether voluntary acquisition resumed; survey attitudes associating the old script with shame or religiosity.',
    'If internalized, effective suppression exceeds what the legal record shows — the constraint travels inside speakers after the enforcement machinery weakens, and the falling suppression_requirement series overstates liberalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(old_script_stigma_internalization, empirical, 'Structural versus internalized maintenance of the old script''s exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(orth_tr_t5, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(orth_tr_t15, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(orth_tr_t20, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(orth_tr_t30, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(orth_be_t5, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 5, 0.83).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(orth_be_t15, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(orth_be_t20, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(orth_be_t30, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(orth_su_t5, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 5, 0.84).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement(orth_su_t15, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(orth_su_t20, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(orth_su_t30, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__continuity_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel__instrumentalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the script reform' conflates three structurally distinct claims about what makes an orthography legitimate. continuity_reading (legitimacy from preserved access to historical, religious, and literary tradition), instrumentalist_reading (legitimacy from literacy maximization and administrative efficiency), and this modernist_reading (legitimacy from alignment with European modernity and rupture from the Ottoman-Islamic past) each carry their own epsilon, victim sets, and classification. They are linked here as family members, not merged: the upstream modernist claim was historically cited as warrant for downstream instrumentalist defenses of the accomplished fact, while the continuity reading registers the same statute as uncompensated destruction. Downstream of this family sits the word-purification movement, which presupposes the script break and is left to its own story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__modernist_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
