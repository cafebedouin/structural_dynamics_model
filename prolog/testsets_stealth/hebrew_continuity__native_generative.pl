% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Native-Generative Criterion of Hebrew Continuity
 *   domain: sociolinguistic/political
 *
 * SUMMARY:
 *   The constraint 'Hebrew lives only through native speaker intuition and
 *   daily generative use' is the native_generative reading of the contested
 *   kernel hebrew_continuity — the question of what constitutes Hebrew being
 *   alive. This reading, institutionalized by the Zionist revival movement
 *   and administered since by the Academy of the Hebrew Language and the
 *   state education system, produced the most successful language revival on
 *   record: a single spoken standard built from mutually unintelligible
 *   immigrant populations and millions of native speakers within three
 *   generations. The same criterion issued a verdict on every other mode of
 *   Hebrew life: liturgical-only communities' Hebrew was classified as dead,
 *   their pronunciation stigmatized as diaspora residue, their continuity
 *   claim annulled. The claim/metrics gap is deliberate and is the datum: the
 *   reading CLAIMS the criterion is descriptive linguistics — what language
 *   life simply is — while the authored metrics describe an actively enforced
 *   standard with a real coordination achievement and real extraction from
 *   the communities the criterion reclassified. Sibling readings
 *   (liturgical_preservation, bridge_pidginized) are separate constraints
 *   with their own epsilon values, beneficiaries, and victims; see
 *   network.dual_formulation_note and the kernel_reading_contestation omega.
 *
 * KEY AGENTS:
 *   - secular_zionist_revivalists: agenda-setting movement (institutional/identity_locked) — built the criterion and enforced it; identity fused with it
 *   - hebrew_language_academy: administering body (institutional/identity_locked) — runs the standard and collects authority from it
 *   - israeli_native_speakers: primary beneficiary (organized/constrained) — holds exclusive title to living Hebrew under the criterion
 *   - sephardi_pronunciation_communities: secondary beneficiary with payer residue (moderate/constrained) — phonology selected as standard, internal variants flattened
 *   - liturgical_only_communities: primary target (organized/identity_locked) — Hebrew classified dead, pronunciation stigmatized, continuity claim annulled
 *   - ashkenazi_pronunciation_speakers: secondary target (moderate/constrained) — inherited pronunciation displaced and socially marked
 *   - yiddishist_ladinoist_movements: excluded rival account (moderate/trapped) — rival theory of Jewish linguistic life kept out of the classification conversation
 *   - sociolinguistic_researchers: analytical observer (analytical/analytical) — documents registers, tests the criterion, contests the continuity claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.58).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.48).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Native-Generative Criterion of Hebrew Continuity").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistic/political").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '735cdbb6-05b2-42d3-b181-47b8bbd29d26').
narrative_ontology:cs_kernel_codification('735cdbb6-05b2-42d3-b181-47b8bbd29d26', formalized).
narrative_ontology:cs_authority_grounding('735cdbb6-05b2-42d3-b181-47b8bbd29d26', practice).
narrative_ontology:cs_interpretation_layer_present('735cdbb6-05b2-42d3-b181-47b8bbd29d26').
narrative_ontology:cs_reading_relation('735cdbb6-05b2-42d3-b181-47b8bbd29d26', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('735cdbb6-05b2-42d3-b181-47b8bbd29d26', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('735cdbb6-05b2-42d3-b181-47b8bbd29d26', foundational, language_life_requires_native_generative_use).
narrative_ontology:cs_axiom_status(language_life_requires_native_generative_use, holdable).
narrative_ontology:cs_axiom_grounding('735cdbb6-05b2-42d3-b181-47b8bbd29d26', language_life_requires_native_generative_use, empirically_contingent).
narrative_ontology:cs_axiom('735cdbb6-05b2-42d3-b181-47b8bbd29d26', secondary, register_restricted_use_is_not_vitality).
narrative_ontology:cs_axiom_status(register_restricted_use_is_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('735cdbb6-05b2-42d3-b181-47b8bbd29d26', register_restricted_use_is_not_vitality, empirically_contingent).
narrative_ontology:cs_reference_frame('735cdbb6-05b2-42d3-b181-47b8bbd29d26', native_speech_community_baseline).
narrative_ontology:cs_drift_state('735cdbb6-05b2-42d3-b181-47b8bbd29d26', contemporary_critical_sociolinguistics_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('735cdbb6-05b2-42d3-b181-47b8bbd29d26', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_native_speakers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, sephardi_pronunciation_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_only_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, ashkenazi_pronunciation_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, sephardi_pronunciation_communities).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, native_speaker_criterion_of_language_life).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, revival_possibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The movement of teachers, writers, and national institutions that, from the 1880s onward, made Hebrew the vernacular of the Jewish national home: built the Hebrew school network, raised the first native-speaking households, and normalized daily use in workplaces, army, and press. Their personal and national identity is fused with the project and with the criterion that native generative use is what makes Hebrew live; renouncing that criterion would unravel the resurrection narrative at the center of their self-conception.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, secular_zionist_revivalists, agenda_setter,
    institutional, generational, identity_locked, national).

% The statutory body (successor to the Hebrew Language Committee) that administers the standard: decides orthography, grammar, and official coinages, and advises government and education. Its mandate, budget, and scholarly prestige exist because the standard requires ongoing administration; many of its coinages go unadopted while its rulings on official and educational use bind. It both runs the standard and collects authority from it.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academy, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, hebrew_language_academy, beneficiary).

% The speech community raised in Hebrew as a first language, now millions across Israel and diaspora centers. Under the criterion they hold exclusive title to living Hebrew; the annulment of rival claims accrues to them as linguistic capital and status. Their schooling, military service, and public life run through the standard; leaving means emigration and bilingual marginality, and the language's status travels with them wherever they go.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_native_speakers, beneficiary,
    organized, generational, constrained, global).

% Communities whose Sephardi phonological tradition was selected as the base of the standard pronunciation, displacing the Ashkenazi variant that had dominated European print and scholarship. The selection elevated their phonology to national prestige, but subsequent standardization flattened their own internal variants, and later Mizrachi immigrants were pressed toward the school standard rather than toward their own traditions. They gained the crown and lost the autonomy of their own usage.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sephardi_pronunciation_communities, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, sephardi_pronunciation_communities, payer).

% Communities — Haredi enclaves in Israel and traditional communities across the diaspora — whose Hebrew is confined to prayer, scripture, and study and transmitted through liturgy rather than native child acquisition. Under the criterion their Hebrew is classified as dead; their pronunciation is marked as diaspora residue and stigmatized in the public sphere; their claim to unbroken linguistic practice across two millennia is annulled. Their practice is constitutive of religious identity and will not be abandoned; they maintain parallel school systems to preserve it.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_only_communities, payer,
    organized, civilizational, identity_locked, global).

% Speakers of the Ashkenazi Hebrew pronunciation that dominated European scholarship and print for centuries. The standard adopted Sephardi-base phonology; Ashkenazi pronunciation became socially marked, associated with the stigmatized religious sector, and displaced from broadcasting, schooling, and public life. Most speakers code-switch to the standard in public while retaining the inherited pronunciation in prayer.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, ashkenazi_pronunciation_speakers, payer,
    moderate, generational, constrained, national).

% Movements (YIVO, the Bund, Ladinoist cultural associations) that held a rival account of Jewish linguistic vitality: Yiddish or Ladino as the living Jewish vernaculars, Hebrew as a liturgical inheritance, and the vernacular gap to be solved with the existing vernaculars rather than by reviving Hebrew. They had no seat in the Yishuv and Mandate bodies that classified Hebrew's life; their program lost the institutional contest and survives in academic and cultural niches.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, yiddishist_ladinoist_movements, excluded,
    moderate, generational, trapped, global).

% Scholars of language revitalization, language death, and the sociology of language who study the Hebrew case: documenting pre-revival registers of Hebrew use, testing the native-speaker criterion against other revitalization cases, and contesting the continuity claim through the hybrid-language thesis. They take no side in the national project; their analyses are available to every seat.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sociolinguistic_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, israeli_native_speakers).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Built and now maintains a unified spoken Hebrew: one standard pronunciation and an expanding lexicon for immigrant populations from mutually unintelligible language backgrounds, taught through a state school system and normalized through army, press, and daily commerce. The criterion tells every institution what counts as Hebrew being alive, so curriculum, coinage, and status decisions have a single target.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and status from liturgical-only and variant-pronunciation communities to the native speech community and its standardizing institutions; moves institutional authority, educational mandate, and prestige to the Academy, the school system, and the national institutions that administer the standard.
% ABSENT_VOICES: Diaspora liturgical and traditional communities — the populations classified as speakers of a dead language — had no seat in the Yishuv and Mandate-era bodies that issued the verdict; Yiddishist and Ladinoist movements with rival accounts of Jewish linguistic life were excluded from the conversation entirely; traditional pronunciation communities (Yemenite, Ashkenazi) had their variants adjudicated by bodies they did not sit on.
% DISAPPEARANCE_RATIONALE: Daily speech would continue — millions of native speakers are now self-sustaining — but the status order would rearrange: the Academy's mandate, the school system's language ideology, and the exclusivity of the native community's claim to Hebrew continuity would dissolve; liturgical communities would regain standing as continuous if register-restricted speakers; and the revival's founding narrative, the resurrection of a dead tongue, would lose its premise.
% FOUNDING_PROBLEM: In the 1880s Hebrew was a liturgical and literary language without native speakers; the Zionist project required a shared vernacular for immigrants from mutually unintelligible language backgrounds and a national language for nation-building. The criterion was the operational answer: produce native speakers, standardize pronunciation, expand the lexicon.
% FOUNDING_PROBLEM_CORROBORATION: Contemporaneous non-Zionist observers and later critical sociolinguists attest both the problem and its status: Yiddishist institutions (YIVO) documented the same communicative gap while proposing a rival solution; historical sociolinguists of the revival working outside the Israeli establishment confirm the pre-1881 absence of native child speakers; critical linguists such as Ghil'ad Zuckermann attest the problem's reality while contesting the criterion and the continuity claim. No corroborating source outside the beneficiary set attests that the founding problem is still live.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-to-substantial: the criterion's operation annulled the continuity claims of liturgical-only communities and displaced variant pronunciations, while the coordination achievement delivered through the same structure is enormous and real — which is why the type is claimed as tangled_rope: genuine collective-action solution and asymmetric extraction through one arrangement. Suppression (0.48) is institutional and social rather than violent: compulsory schooling, army normalization, broadcast norms, and the stigma of the diaspora accent; it is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. The suppression_requirement series traces enforcement-capacity change because that is the dynamic this story tracks: deliberate build-up (0.20 in the movement's voluntary era, peaking at 0.70 in the state-consolidation decades when the school system, army ulpanim, and broadcast standard were constructed) followed by decay (0.48 today) as the standard became self-reproducing. Theater (0.38, rising from 0.12) tracks Goodhart drift: with the founding problem solved, a growing share of the apparatus's activity is narrative maintenance — Academy coinages that go unadopted, revival ceremonies, restatements of the dead-Hebrew verdict — rather than functional standardization. Accessibility_collapse (0.62): within the reading's frame, once the criterion is accepted, alternatives collapse substantially, but liturgical practice persists in identity-locked enclaves, so collapse is incomplete. Resistance (0.55): the Haredi parallel school system, retained liturgical pronunciations, historical Yiddishist opposition, and contemporary critical sociolinguistics. All three metric series run on one shared time grid (t=0 to 140 by 20) with every metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seats (revivalists, Academy) the criterion is descriptive truth: the reading's own lights see the arrangement as the criterion's successful implementation, and the dead-Hebrew verdict as accurate classification rather than harm. From the payer seats (liturgical_only_communities, ashkenazi_pronunciation_speakers) the same structure operates as delegitimization: a status order that annuls their claim and stigmatizes their practice, administered by institutions they did not sit on. Inter-institutionally, the Academy, the state school system, and the Haredi parallel system experience the same standard from different positions — the first two administer it, the third walls itself against it while remaining inside its verdicts. The excluded rival account (Yiddishist) would reframe the whole operation as a nationalist selection among available Jewish vernaculars dressed as linguistics. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: israeli_native_speakers (exclusive title to living Hebrew — near the beneficiary end), hebrew_language_academy (mandate, budget, and prestige from administering the standard), sephardi_pronunciation_communities (phonology elevated to the standard). Targets: liturgical_only_communities (identity-locked and organized, but their annulled claim and stigmatized practice place them near the full-target end despite real institutional density), ashkenazi_pronunciation_speakers (constrained — public code-switch pressure). Same-level divergence: sephardi and ashkenazi pronunciation communities hold the same nominal power and scope yet sit on opposite sides of the derivation, because the phonological selection that benefited one displaced the other — a constraint-specific factor, not a power difference. The sephardi seat is genuinely dual-positioned: declared beneficiary, but the same standardization that elevated its phonology flattened its internal variants and was later applied against its own communities' traditions; no directionality override is authored because the override mechanism is power-atom-keyed and would misapply to the other moderate seats — the dual position is carried here and in the stakeholder situation instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no shared Jewish vernacular — is dead: solved by the mid-twentieth century. The arrangement persists, and what persists is increasingly the status apparatus (who counts as heir to Hebrew) rather than the vernacular-building function. Authoring founding_problem_status=dead together with disappearance_verdict=world_rearranges is the honest mismatch: the institutional and status architecture depends on the criterion even though daily speech no longer does, and the mismatch consumer should flag this for capture/zombie cross-check against the rising theater series. The classification prevents two mislabels: reading the arrangement as pure rope (the establishment's self-description — we merely stated what language life is) erases the extraction from the liturgical and Ashkenazi-pronunciation seats; reading it as snare erases the genuine coordination achievement the same structure delivered. The tangled_rope claim keeps both in view. The drift risk is toward piton — theater rising, founding problem dead — but extraction remains concentrated enough (named victims, named capturers, an identifiable seat the gains accrue to) that the structure is still a tangled rope today rather than an inertial remainder.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the native_generative reading of the hebrew_continuity kernel; what structurally changes under the sibling readings, and where exactly do the readings disagree?',
    'No data resolves a conceptual contest; resolution would be an explicit adjudication of the kernel — for example, a pluralist official account of Hebrew vitality naming which criterion governs which decision (education, status recognition, funding).',
    'Under liturgical_preservation the victim set inverts: the standardizing state becomes the party whose verdict machinery imposes on preserving communities, and this arrangement''s extraction is read from that seat as substantially higher. Under bridge_pidginized both the native and liturgical claims become partial and the exclusivity extraction collapses. The disagreement is located in the sufficiency operator: whether native generative use is necessary, sufficient, or merely one mode of language life.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three readings of the hebrew_continuity kernel; sibling readings restructure the victim set.').

omega_variable(
    criterion_naturalness_ambiguity,
    'Is the native-speaker criterion a natural fact about language (all vital languages have native speakers, so the criterion merely states what linguistic life is) or a constructed standard serving nation-building whose force derives from state enforcement?',
    'Comparative revitalization analysis: cases where communities sustain register-restricted or second-language-dominant vitality without native child acquisition (liturgical continuities, Arabic diglossia, Latin scholarship) test whether the criterion is definitional or one empirical regularity among others.',
    'If the criterion is natural fact, the arrangement''s extraction is confined to its enforcement excesses; if constructed, the dead-Hebrew verdict is a policy classification with identifiable beneficiaries, and the constraint''s extractiveness is structural rather than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_naturalness_ambiguity, conceptual, 'Whether the criterion is descriptive linguistics or a constructed national standard.').

omega_variable(
    pre_revival_vitality_status,
    'Was pre-1881 Hebrew actually dead — or register-restricted alive — given documented use in prayer, study, responsa correspondence, trade pidgins, and some Sephardi communal vernacular use?',
    'Sociolinguistic documentation of pre-revival Hebrew use registers: volume, domains, intergenerational transmission, and productive rather than merely receptive use in each community.',
    'If register-restricted use counts as partial vitality, the ''revival'' is register expansion, the dead-language verdict was false when issued, and the extraction from liturgical-only communities rested on a false premise — raising effective extraction; if dead, the verdict was accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_revival_vitality_status, empirical, 'Empirical status of pre-revival Hebrew vitality underlying the dead-language verdict.').

omega_variable(
    phonological_standardization_necessity,
    'Was the displacement of the Ashkenazi pronunciation, and the flattening of Sephardi internal variants, a necessary coordination cost of building one speech community — or extraction that a multi-pronunciation equilibrium could have avoided?',
    'Counterfactual comparison with multi-accent vital languages (English, the Arabic register continuum, Spanish): whether a revived Hebrew could have sustained mutual intelligibility and institutional function with variant pronunciations.',
    'If a multi-pronunciation equilibrium was viable, the displacement component of the arrangement is extraction rather than coordination cost, and the ashkenazi_pronunciation_speakers seat''s effective extraction is structural; if not, it prices into the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonological_standardization_necessity, conceptual, 'Whether phonological standardization was coordination cost or extractive displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__native_generative, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__native_generative, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__native_generative, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t80, observed).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.32).
narrative_ontology:measurement_basis(hebr_tr_t100, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_continuity__native_generative, theater_ratio, 120, 0.35).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).
narrative_ontology:measurement(hebr_tr_t140, hebrew_continuity__native_generative, theater_ratio, 140, 0.38).
narrative_ontology:measurement_basis(hebr_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__native_generative, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__native_generative, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(hebr_be_t40, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.6).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__native_generative, base_extractiveness, 80, 0.62).
narrative_ontology:measurement_basis(hebr_be_t80, observed).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(hebr_be_t100, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_continuity__native_generative, base_extractiveness, 120, 0.56).
narrative_ontology:measurement_basis(hebr_be_t120, observed).
narrative_ontology:measurement(hebr_be_t140, hebrew_continuity__native_generative, base_extractiveness, 140, 0.58).
narrative_ontology:measurement_basis(hebr_be_t140, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__native_generative, suppression_requirement, 20, 0.32).
narrative_ontology:measurement_basis(hebr_su_t20, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__native_generative, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(hebr_su_t40, observed).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.7).
narrative_ontology:measurement_basis(hebr_su_t60, observed).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__native_generative, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(hebr_su_t80, observed).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.52).
narrative_ontology:measurement_basis(hebr_su_t100, observed).
narrative_ontology:measurement(hebr_su_t120, hebrew_continuity__native_generative, suppression_requirement, 120, 0.46).
narrative_ontology:measurement_basis(hebr_su_t120, observed).
narrative_ontology:measurement(hebr_su_t140, hebrew_continuity__native_generative, suppression_requirement, 140, 0.48).
narrative_ontology:measurement_basis(hebr_su_t140, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial claim 'Hebrew is alive' decomposes into three structurally distinct constraints per the epsilon-invariance principle. native_generative (this story): life requires native child acquisition and daily generative use; epsilon ~0.58; victims are liturgical-only and variant-pronunciation communities. liturgical_preservation: ritual recitation and textual transmission sustain life; under its own lights the standardizing state is the imposing party and the victim set inverts. bridge_pidginized: contact-language use sustains life; both the native and liturgical claims become partial. Each has its own epsilon, beneficiaries, and victims. The upstream reading (native_generative, backed by state power and a realized speech fact) structurally influences the standing of the other two, because its verdict machinery is what classifies their practices as living or dead; all three stories link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
