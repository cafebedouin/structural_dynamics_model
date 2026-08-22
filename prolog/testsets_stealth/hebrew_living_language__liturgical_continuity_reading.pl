% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Liturgical Continuity of Hebrew Across the Diaspora
 *   domain: historical_linguistics/cultural
 *
 * SUMMARY:
 *   From the consolidation of the rabbinic canon (c. 200 CE) to the eve of
 *   the modern revival (1880), Hebrew's continuity rested on a distributed
 *   practice: daily and sabbath liturgy recited in Hebrew, and a text-study
 *   system — Bible with masorah, Mishnah, Talmud, and their commentaries —
 *   sustained by every diaspora community from the Rhineland to Babylon. This
 *   story instantiates the liturgical_continuity_reading of the
 *   hebrew_living_language kernel: the claim that this unbroken practice
 *   constituted Hebrew remaining a living language, not a relic awaiting
 *   resurrection. The ε referent is the standing diaspora arrangement itself,
 *   assessed by this reading's own lights — not the native-speech arrangement
 *   the later revival produced, and not either sibling reading's standard.
 *   The claim is authored as rope from the structure (voluntary
 *   participation, no victim seat, a real coordination good: a shared
 *   textual-liturgical channel across vernacular-diverse communities); the
 *   metrics are authored independently from the descriptive historical
 *   record, and their consistency with the claim is an outcome of the
 *   history, not a tuning target. Sibling readings —
 *   native_generation_reading and literary_revival_reading — are separate
 *   constraints; the definitional contest is routed to the omega variables,
 *   and the sibling constraint IDs used in network edges assume the
 *   kernel__reading naming pattern.
 *
 * KEY AGENTS:
 *   - rabbinical_leadership: agenda-setter and principal status-beneficiary (institutional / identity_locked) — canonizes the liturgy and curriculum, administers the transmission chain, collects deference and authority
 *   - yeshiva_and_heder_educators: beneficiary via livelihood (moderate / constrained) — decades of textual training convert into teaching standing; exit forfeits it
 *   - synagogue_worshippers: net beneficiary bearing the practice's time cost (moderate / constrained) — recites, often with partial comprehension, for membership and identity
 *   - diaspora_jewish_communities: collective beneficiary (organized / constrained) — fund schools and receive the intercommunal channel the practice maintains
 *   - vernacular_advocates: excluded dissenting seat (organized / mobile) — Yiddishists and secular skeptics contest the continuity claim from outside the rabbinic framework
 *   - historical_linguists: analytical observer (analytical / analytical) — measure what the chain preserved and define the measurement the siblings dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.18).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.18).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Liturgical Continuity of Hebrew Across the Diaspora").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, 'b8fa83fc-1ab4-411a-92e3-48f06bb5e706').
narrative_ontology:cs_kernel_codification('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', fixed_text).
narrative_ontology:cs_authority_grounding('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', lineage).
narrative_ontology:cs_interpretation_layer_present('b8fa83fc-1ab4-411a-92e3-48f06bb5e706').
narrative_ontology:cs_reading_relation('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', foundational, transmitted_practice_constitutes_liveness).
narrative_ontology:cs_axiom_status(transmitted_practice_constitutes_liveness, holdable).
narrative_ontology:cs_axiom_grounding('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', transmitted_practice_constitutes_liveness, conventional).
narrative_ontology:cs_axiom('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', secondary, covenantal_transmission_obligation).
narrative_ontology:cs_axiom_status(covenantal_transmission_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', covenantal_transmission_obligation, theological).
narrative_ontology:cs_reference_frame('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', unbroken_masoretic_continuum).
narrative_ontology:cs_drift_state('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', post_revival_linguistic_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b8fa83fc-1ab4-411a-92e3-48f06bb5e706', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, rabbinical_leadership).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, yeshiva_and_heder_educators).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, synagogue_worshippers).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__liturgical_continuity_reading, synagogue_worshippers).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, hebrew_continuity_thesis).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, liturgical_transmission_adequacy).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, masoretic_textual_fidelity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Canonizes the liturgy and the study curriculum, ordains communal educational ordinances, and administers the transmission chain from the Geonic academies through the medieval yeshivot. Deference, communal funds, and interpretive authority flow to this seat as guardian of the language and texts; the role is constituted by the chain it administers, so leaving it would dissolve the seat itself rather than relocate its holder.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, rabbinical_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, rabbinical_leadership, beneficiary).

% Make their livelihood teaching Hebrew texts to boys in heder and yeshiva. Decades of textual training convert into teaching standing and communal honor; alternative livelihoods of equal standing are few, and exit means forfeiting status built entirely on mastery of the canon.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, yeshiva_and_heder_educators, beneficiary,
    moderate, biographical, constrained, regional).

% Recite the liturgy daily and weekly, often with partial comprehension of the Hebrew they reproduce, gaining membership, ritual access, and identity. They bear the time cost of the practice and the communal expectation of textual literacy; exit into full vernacular life was possible but carried communal and familial cost.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, synagogue_worshippers, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__liturgical_continuity_reading, synagogue_worshippers, payer).

% Self-governing kehillot from the Rhineland to Babylon fund schools, tax members for teachers, and receive the coordination good: a shared language of prayer, law, and correspondence binding communities that share no spoken vernacular. Exit existed through assimilation or conversion but was catastrophic and rare.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, continental).

% Yiddishists, maskilic critics of heder rote, and later secular skeptics argue that the language of daily life is the living one and that recited liturgy is inherited sound. They publish, organize, and contest the continuity claim from outside the rabbinic framework that sustains this reading, with no seat in the councils that administer the practice.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, vernacular_advocates, excluded,
    organized, biographical, mobile, continental).

% Reconstruct what the transmission chain preserved — masoretic phonology, morphology, lexicon, textual fidelity — and where reading traditions diverged. They take no side in the communal dispute but define the measurement (what counts as a living language) that the sibling readings contest.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, rabbinical_leadership).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared textual-liturgical channel across dispersed, vernacular-diverse communities: prayer, script, and canon let communities with no common spoken language correspond, adjudicate, and read the same texts across centuries.
% TRANSFER_FUNCTION: Moves study labor and communal funds from households into the textual economy; moves status and interpretive authority to demonstrated Hebrew mastery; moves the canon itself — copied, recited, interpreted — across generations.
% ABSENT_VOICES: Vernacular advocates and later academic skeptics of the continuity claim would object that recited liturgy without generative speech is not a living language; they stood outside the rabbinic framework — in the Yiddish press, the maskilic critique of heder rote, and later the academy. Lay worshippers who experienced the liturgy as opaque sound were present in the pews, but their experience was not admitted as data by the seats that administered the practice.
% DISAPPEARANCE_RATIONALE: Over the interval, the practice was the channel: without it, the canon goes uncopied and unread, the intercommunal correspondence network collapses, the masoretic text tradition frays, and the nineteenth-century revival loses both its literate base and its standardized corpus — the language history of the diaspora rearranges around the practice's absence. Post-revival, the linguistic load-bearing has shifted to native speech; the practice would still matter liturgically, but no longer linguistically.
% FOUNDING_PROBLEM: After Hebrew ceased to be anyone's daily vernacular in late antiquity, the communities faced the problem of keeping the language — and the texts constituted in it — alive without native transmission.
% FOUNDING_PROBLEM_CORROBORATION: The problem's existence is corroborated from outside the beneficiary set: historical linguists and revival scholars uniformly attest that Hebrew lacked native speakers before the modern revival. What they dispute is the solution — how much the liturgical chain, versus the European linguistic substrate, contributed to the revival's success. No serious source disputes that the founding problem was real; the contest is over which arrangement solved it.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18 at interval end) because the practice's costs — study hours, communal education taxes, the opportunity cost of literacy in a language of no commerce — were borne by communities that simultaneously collected the good: identity, textual access, and a correspondence channel no vernacular provided. Suppression is equally low (0.18) and structurally so: the practice never closed alternatives, since every participant also lived in a vernacular (Aramaic, Greek, Arabic, Yiddish, Ladino); it obligated a practice without monopolizing a function. Theater is low (0.20) because the recitation was the transmission — phonology, morphology, and the masoretic text passed through it whether or not any given reciter comprehended — though the series shows a slow rise as elite mastery and lay rote recitation diverged. Accessibility_collapse is 0.30: exit into full vernacular life was real and exercised by individuals and whole segments (vernacular prayer reform, the Yiddishist counter-program), and never coercively foreclosed. Resistance is 0.35: the maskilic critique, the Yiddishist movement, and later academic skepticism met the continuity claim without threatening the practice's religious core. The suppression_requirement series traces enforcement-capacity change, not extraction: communal discipline (compulsory heder ordinances, education taxes, herem threats) built from the Geonic period to a medieval peak (~0.38 at 1400) and dissolved with emancipation and state schooling (0.18 by 1880), after which participation was voluntary. All three series share one time grid (200, 500, 800, 1100, 1400, 1700, 1880). Receipt surface: the arrangement's concentrated surplus — communal funds, deference, canon control — demonstrably accrues to the rabbinical seat (gain_flow: rabbinical_leadership), while fixing (dismantling the liturgical-educational complex) would cost a civilization its core practice against a trivial extraction benefit (fixing_cost: prohibitive).
 *
 * PERSPECTIVAL GAP:
 *   The rabbinical seat experiences the arrangement as covenantal continuity it stewards — the same texts, the same recitation, an unbroken chain; from that seat the constraint is nearly costless because the practice IS the seat's identity. The lay worshipper experiences partial comprehension and memorized recitation — the good (membership, ritual access) is real, but the linguistic content is often opaque, which is precisely the experience the native-generation sibling elevates into a definitional objection. The excluded vernacular advocate experiences the continuity claim as mystification — a literary relic dressed as a living tongue — and organizes against it. The historical linguist experiences the arrangement as a transmission dataset: phonology preserved, pronunciation diverged, comprehension stratified. One structure, four different constraints; the engine computes per-seat classifications from the structural data, and the divergence here is definitional (what counts as 'living'), not merely experiential.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party derives a low directionality: the practice subsidizes identity, cohesion, and textual access to the same communities that bear its costs, so no high-d target seat exists — consistent with the rope claim and with the expected structural delta (no victim set). rabbinical_leadership sits nearest the beneficiary end (agenda-setter collecting deference and canon control); diaspora_jewish_communities and yeshiva_and_heder_educators derive low d from their beneficiary declarations. One override is authored: synagogue_worshippers are declared beneficiaries but also bear the practice's real time cost and often recite without full comprehension; the derivation from the primary beneficiary role would place them too near d=0, so d is corrected to 0.35 — still beneficiary-side, honestly weighted. vernacular_advocates appear in no beneficiary or victim array: they rejected the good rather than being extracted from, and their seat is exclusion, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding linguistic problem — sustaining Hebrew without native transmission — is dead: the revival restored native speech, so the arrangement persists without its linguistic necessity. Authored honestly, founding_problem_status=dead combined with disappearance_verdict=world_rearranges fires the mismatch flag; the cross-check against the computed path should not confirm a zombie: theater is low (0.20), no seat captures concentrated extraction beyond modest status gains to the rabbinate, and the practice's primary function — worship and study as covenantal obligation — never depended on the linguistic function and remains live. The classification work this story does is to prevent two mislabelings: a snare reading (communities coerced into maintaining a dead language for elite benefit) fails for want of any victim seat; a piton reading (theatrical maintenance of an atrophied function) fails because the recitation retained its transmission function throughout the interval and its religious function still. The mandatrophy risk sits not in the practice but in the claim: after 1880 the claim's linguistic load-bearing transfers to the native-generation sibling, and this reading survives as the historical account of how the substrate was kept alive — which is what the minor, acknowledged practice_drift in cs_structure records.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is the liturgical_continuity_reading of the hebrew_living_language kernel. What would change if the native_generation_reading''s definitional standard — liveness requires native generative daily speech — were adopted as the measurement?',
    'Adopting the sibling standard would re-author ε against the same arrangement (the diaspora practice) under a stricter referent: recitation without generative competence would score as preserved symbol rather than living language, and the classification would be recomputed — plausibly toward a piton-flavored verdict for the rote-recitation layer.',
    'The same historical practice would instantiate a different constraint with higher theater and a contested continuity claim; the rope verdict here is reading-indexed, not arrangement-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one of three readings of the Hebrew-liveness kernel; the definitional dispute with the native-generation sibling.').

omega_variable(
    recitation_comprehension_share,
    'What share of reciters, across the interval, comprehended the Hebrew they recited rather than reproducing memorized sound?',
    'Responsa literature on whether prayer may be said in the vernacular (and the permission''s scope), the deployment of translations (Onkelos, Rashi, later vernacular siddur glosses), and reconstructed curricula recover the comprehension distribution by class and period.',
    'A low comprehension share raises the effective theater of the recitation layer and strengthens the native-generation critique; a high share (via the study system) supports this reading''s claim that recitation and study were one practice, not recitation alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recitation_comprehension_share, empirical, 'Comprehension distribution underlying the recitation practice.').

omega_variable(
    phonological_transmission_uniformity,
    'Did the liturgical chain transmit a single language system, or parallel reading traditions diverged beyond mutual intelligibility?',
    'Comparative dialectology of the masoretic reading traditions (Babylonian, Tiberian, Ashkenazi, Sephardi, Yemenite) and their mutual intelligibility for biblical and rabbinic texts.',
    'Documented divergence shows the chain preserved phonological structure and textual fidelity without uniform pronunciation — supporting continuity of the language system while complicating any ''unbroken and uniform'' reading; the continuity claim survives as system-level, not uniformity-level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonological_transmission_uniformity, empirical, 'Whether transmission preserved one system or diverged traditions.').

omega_variable(
    counterfactual_transmission_necessity,
    'Was the communal liturgical-study practice necessary for Hebrew''s survival as a usable language, or would elite scholarly use alone (the Latin trajectory) have sufficed for the later revival?',
    'Comparative case analysis: Aramaic (native speakers but fading institutional base), Coptic and Syriac (liturgical survival without revival), Latin (scholarly survival, partial revival) — what distinguished the cases that revived.',
    'If the communal practice was load-bearing, the rope''s coordination claim is genuine and the low extraction was the price of a real good; if scholarly use alone would have sufficed, the practice''s linguistic coordination claim is inflated and the reading rests on the religious function alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_transmission_necessity, empirical, 'Counterfactual necessity of the communal practice for the revival''s inputs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 200, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_lit_continuity_tr_t200, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement(heb_lit_continuity_tr_t500, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(heb_lit_continuity_tr_t800, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(heb_lit_continuity_tr_t1100, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(heb_lit_continuity_tr_t1400, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(heb_lit_continuity_tr_t1700, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1700, 0.18).
narrative_ontology:measurement(heb_lit_continuity_tr_t1880, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 1880, 0.2).

% Extraction over time
narrative_ontology:measurement(heb_lit_continuity_be_t200, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 200, 0.1).
narrative_ontology:measurement(heb_lit_continuity_be_t500, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 500, 0.14).
narrative_ontology:measurement(heb_lit_continuity_be_t800, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 800, 0.18).
narrative_ontology:measurement(heb_lit_continuity_be_t1100, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1100, 0.22).
narrative_ontology:measurement(heb_lit_continuity_be_t1400, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1400, 0.25).
narrative_ontology:measurement(heb_lit_continuity_be_t1700, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1700, 0.22).
narrative_ontology:measurement(heb_lit_continuity_be_t1880, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 1880, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(heb_lit_continuity_su_t200, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 200, 0.15).
narrative_ontology:measurement(heb_lit_continuity_su_t500, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 500, 0.2).
narrative_ontology:measurement(heb_lit_continuity_su_t800, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 800, 0.28).
narrative_ontology:measurement(heb_lit_continuity_su_t1100, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1100, 0.35).
narrative_ontology:measurement(heb_lit_continuity_su_t1400, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1400, 0.38).
narrative_ontology:measurement(heb_lit_continuity_su_t1700, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1700, 0.25).
narrative_ontology:measurement(heb_lit_continuity_su_t1880, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 1880, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% The colloquial claim 'Hebrew is a living language' decomposes, per the ε-invariance principle, into three structurally distinct constraints sharing one kernel: liturgical continuity (this file — transmission practice, low ε), literary revival (written generative competence without daily speech), and native generation (daily generative speech — the strict standard). The readings differ on the definition of 'living,' so each instantiates a different constraint with its own ε, beneficiaries, and classification; measuring one with another's observable would change ε and is therefore forbidden inside a single story. This reading is historically upstream: the liturgically-literate population and the standardized masoretic corpus it transmitted are the substrate the literary revival drew on, hence the influences edge to the revival sibling; the native-generation sibling is a definitional rival held by other parties, hence coexists_with. Sibling constraint IDs follow the kernel__reading pattern and are declared as network targets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language__liturgical_continuity_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
