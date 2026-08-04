% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Linguistic Life via Liturgical Preservation Chain
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_preservation_reading of the
 *   contested hebrew_linguistic_life kernel. The reading asserts: a language
 *   is alive when its sacred texts are continuously recited, studied, and
 *   transmitted in an unbroken chain of authority and practice, regardless of
 *   whether the language functions as a living vernacular for everyday
 *   speech. Under this reading, Hebrew never died — it has been continuously
 *   alive through the chain of liturgical practice stretching from Biblical
 *   times through the Rabbinic period, the medieval diaspora, and into the
 *   modern era. Ben-Yehuda's late-19th-century project to resurrect Hebrew as
 *   a native vernacular is reframed under this reading not as resurrection
 *   (implying prior death) but as desecration or replacement: the
 *   construction of a modern language using Hebrew's lexical and
 *   morphological materials but fundamentally rupturing the continuous chain
 *   of sacred transmission by inserting a secular, constructed,
 *   non-liturgically-mediated language into domains that sacred Hebrew had
 *   occupied unbroken. The constraint's beneficiary is the institutional
 *   structure that enforces and maintains the liturgical chain — rabbinical
 *   authority, yeshivot, synagogue institutions. Its victim is the integrity
 *   of the sacred tradition itself, which this reading argues is damaged by
 *   competing vernacular claims to be 'Hebrew alive.' This reading directly
 *   contests the other two siblings in the kernel: the
 *   native_generational_reading (which treats vernacular mother-tongue
 *   transmission as the criterion for linguistic life, and thus treats
 *   historical Hebrew as dead until Ben-Yehuda) and the
 *   marketplace_pidgin_reading (which treats functional inter-communal
 *   coordination as the criterion, making liturgical-only Hebrew
 *   linguistically dead by its measure).
 *
 * KEY AGENTS:
 *   - liturgical_authority_structure (institutional beneficiary; includes rabbinical councils, yeshiva systems, synagogue leadership) — maintains the unbroken chain, enforces standards of textual transmission, controls access to authoritative interpretation
 *   - jewish_institutional_continuity (structural beneficiary; the continuity of Jewish peoplehood through diaspora and persecution) — benefits from having a language that survives through institutional preservation independent of political state or demography
 *   - sacred_hebrew_tradition (victim; abstract but institutional) — the tradition's integrity is extracted from by competing claims and vernacularization
 *   - hebrew_vernacular_speakers (victim; modern Hebrew users) — their language is framed as desecration or imposture, their everyday speech disqualified from authentic linguistic life
 *   - ben_yehuda_project_heirs (analytical seat, partly victim under this reading; nation-state project) — the modern Hebrew revival is the target of the constraint's verdict: a constructed impostor, not continuation
 *   - contemporary_israeli_speakers (modern vernacular users; moderate power, trapped exit) — use Hebrew daily for all functions but are excluded from 'linguistic life' under this reading's definition, or included only under sufferance as participants in a constructed language rather than the living chain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.62).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.71).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life via Liturgical Preservation Chain").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '7f166ca2-35b1-43d0-9e44-bac93c6fa1f4').
narrative_ontology:cs_kernel_codification('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', fixed_text).
narrative_ontology:cs_authority_grounding('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', lineage).
narrative_ontology:cs_interpretation_layer_present('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4').
narrative_ontology:cs_reading_relation('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', foundational, liturgical_chain_criterion_aliveness).
narrative_ontology:cs_axiom_status(liturgical_chain_criterion_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', liturgical_chain_criterion_aliveness, deontological).
narrative_ontology:cs_axiom('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', foundational, vernacular_use_incompatible_sacred_transmission).
narrative_ontology:cs_axiom_status(vernacular_use_incompatible_sacred_transmission, holdable).
narrative_ontology:cs_axiom_grounding('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', vernacular_use_incompatible_sacred_transmission, deontological).
narrative_ontology:cs_reference_frame('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', unbroken_liturgical_transmission_framework).
narrative_ontology:cs_drift_state('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', post_ben_yehuda_construction, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f166ca2-35b1-43d0-9e44-bac93c6fa1f4', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authority_structure).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, jewish_institutional_continuity).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_integrity).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_vernacular_diversity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, contemporary_israeli_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_project_heirs).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, contemporary_israeli_speakers).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_fidelity_as_linguistic_life_criterion).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, textual_transmission_chain_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinical councils, yeshiva systems, synagogue leadership, and Talmudic interpreters maintain the standards and gates for textual transmission. They set what counts as authentic Hebrew (textual accuracy, pronunciation preservation, hermeneutical continuity). They control entry into the chain through education and certification. They benefit by maintaining institutional authority over linguistic authenticity and the prestige of gatekeeping. Their 'exit' is to leave the tradition, but institutional actors investing in the preservation function have career dependence on its continuation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authority_structure, agenda_setter,
    institutional, civilizational, mobile, global).

% The broader Jewish institutional apparatus — synagogues, diaspora communities, ethnic and cultural organizations — benefits from a definition of linguistic/cultural life that can persist through dispersion and political powerlessness. The liturgical preservation reading guarantees that Hebrew remains 'alive' even when Jews lack a state, territory, or native speaker base. This provides cultural continuity and grounds for ethnic/religious identity across centuries and geographies. It is not a person but an institutional network; it is listed as an agent only because it is the structural beneficiary of the reading's verdict. It has no exit options — if the reading fails, the institutional network loses one pillar of continuity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, jewish_institutional_continuity, beneficiary,
    institutional, civilizational, analytical, global).

% Not an actor but the tradition itself — the corpus of texts, interpretations, meanings, and practices that constitute the sacred inheritance. This reading extracts from the tradition by confining it to specialists and liturgical contexts, preventing it from evolving naturally in vernacular speech, and rendering it inaccessible to ordinary speakers. The tradition bears the cost of gatekeeping even as gatekeeping claims to preserve it. Listed here as a non-agent (agent: false) to mark that it is named as a victim for narrative completeness but does not hold a seat with decision-making power.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_hebrew_tradition, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_hebrew_tradition).

% Modern Hebrew speakers who use the language for all daily functions — work, family, art, politics, science. They use a language that evolved from Medieval Hebrew, incorporating Yiddish, Aramaic, and modern neologisms, constructed deliberately by Ben-Yehuda and others. Under this reading, their language is not 'truly Hebrew alive' but a constructed impostor. They bear the cost of linguistic delegitimization: their native language is reframed as inauthentic or desecrated. They cannot exit by using another language without abandoning their cultural anchor; they are trapped in a language that the reading classes as inauthentically alive. Their exit options are constrained by identity-lock: to reject Modern Hebrew would be to reject Israeli/diaspora Jewish identity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_vernacular_speakers, payer,
    moderate, biographical, constrained, global).

% The nation-state project (Israel), academic linguistics, secular Hebrew revival movements, and the modern literary/cultural establishment that treats Modern Hebrew as authentically alive. They benefit from treating the language they speak and promote as continuous with Jewish history. Under this reading, their project is the desecration or replacement of the authentic tradition — they are charged with linguistic inauthenticity at the highest level. Their exit options are mobile in principle (they could abandon the Hebrew-aliveness claim and treat it as a constructed language strategically adopted), but institutionally and ideologically they are invested in continuity narratives, so exit is psychologically and politically costly.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_project_heirs, payer,
    powerful, generational, mobile, global).

% Millions of Hebrew speakers who use the language natively for all functions: children, adults, workers, artists. They benefit from the language functioning as a coordinating medium and as the basis of cultural identity. They also bear extraction under this reading: their native language is declared inauthentically alive or a secular construction. They cannot exit by switching languages without abandoning identity and community. They are identity-locked: to speak Hebrew and be Israeli/Jewish requires accepting (under this reading) that they are either speaking an imposture or only participate in authentic linguistic life if they also engage the liturgical chain — which they mostly do not.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, contemporary_israeli_speakers, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, contemporary_israeli_speakers, beneficiary).

% Academic linguists who study Hebrew as a natural language undergoing revival and evolution. They observe the constraint's operation: how institutional authority enforces a particular definition of linguistic aliveness, how competing definitions are suppressed, how the liturgical reading claims authority over authenticity. They take no position in the contest but document the structural dynamics and measure the gap between the readings.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, modern_hebrew_linguists, observer,
    institutional, biographical, analytical, global).

% Not an actor but a reading itself — the position that linguistic life requires mother-tongue acquisition and everyday vernacular transmission. This reading is structurally excluded from the liturgical reading's framework: if you accept that linguistic life is constituted by liturgical chain transmission, you have already foreclosed the native-generational reading's verdict. The constraint actively suppresses this reading's authority. Listed as excluded to mark that it represents a major alternative voice in the kernel contest but is not seated at the negotiating table within this reading's own frame.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, competing_native_generational_reading, excluded,
    powerful, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, competing_native_generational_reading).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authority_structure).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits a sacred language and its textual heritage across diaspora, dispersion, political powerlessness, and centuries of change. Coordinates Jewish identity continuity and cultural transmission without requiring geographic territory, political state, or native speaker base. Ensures textual and doctrinal integrity by gatekeeping interpretation and transmission standards.
% TRANSFER_FUNCTION: Transfers authority over linguistic authenticity and definition from vernacular speakers to institutional gatekeepers (rabbinical councils, yeshivot). Transfers legitimacy and prestige from modern speakers to liturgical specialists. Transfers the language itself from a living vernacular medium (as it was in antiquity) to a specialized ritual and textual medium, requiring modern speakers to adopt or code-switch into other languages (Yiddish, Aramaic, Arabic, modern European languages) for daily life outside the liturgical domain.
% ABSENT_VOICES: The native_generational_reading (modern linguistics, Israel's secular education system, ordinary speakers claiming vernacular aliveness) and the marketplace_pidgin_reading (historical linguists, commercial and diplomatic traditions of Hebrew as a coordinating medium) are structurally excluded from authority within this reading's frame. They would object that the reading defines linguistic life too narrowly and privileges institutional gatekeeping over actual speaker agency. They are kept out by the reading's own premise: once you accept the liturgical chain as the criterion, you have already disqualified their voices.
% DISAPPEARANCE_RATIONALE: If this constraint (the liturgical preservation reading and its institutional enforcement) disappeared: one position (held by the reading's defenders) says Hebrew would become truly dead — without the unbroken chain it would degrade into multiple pidgins and lose its coherence as a unified tradition. Another position (held by the other readings) says the language would flourish as it already has in Modern Hebrew — vernacular acquisition would continue, marketplace functions would persist, and the language's aliveness would be evident in living speakers rather than institutional transmission. The verdict is contested because the disappearance of this constraint means also the disappearance of the institutional authority enforcing the definition, so the contest itself would be altered.
% FOUNDING_PROBLEM: Hebrew must survive as a living, coherent language and religious/cultural medium across diaspora, persecution, loss of territory, and the pressures of assimilation into host-language cultures. Without an unbroken chain of transmission rooted in sacred obligation and institutional practice, the language risks fragmentation into dialects, loss of hermeneutical continuity, corruption of authoritative texts, and eventual disappearance into the languages of diaspora lands.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical authorities and liturgical communities attest the problem is live: without gatekeeping and institutional transmission, sacred tradition degrades and Hebrew loses coherence. Historians of medieval and early modern Judaism attest that Hebrew did survive through the diaspora, and that institutional preservation was critical to survival. Modern Israeli state and secular Hebrew advocates attest that the problem has been substantially transformed: Hebrew survives not through liturgical gatekeeping but through the modern vernacular, native speaker acquisition, and state support — and that the liturgical reading now extracts cost by delegitimizing the vernacular. Independent linguists and anthropologists document that other sacred languages (Latin, Classical Arabic in Quranic contexts) maintain liturgical transmission without native speakers, and that the distinction between 'alive' and 'liturgically preserved' is a matter of definition, not observable fact.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the constraint enforces a gate on what counts as authentic linguistic life, and that gate concentrates gatekeeping authority in institutional hands. Suppression is substantial (0.71) because the reading actively suppresses competing definitions of aliveness — it must suppress the marketplace reading (which treats Modern Hebrew as alive) and the native reading (which also treats Modern Hebrew as alive, but for vernacular reasons). Theater is moderate-high (0.48) because the constraint maintains a performance of unbroken transmission that may obscure discontinuities in actual practice (liturgical Hebrew used only in highly formal contexts by most speakers, not in transmission chains of lived mastery). Accessibility collapse is high (0.79) because once the reading is understood, its implications are clear: ordinary vernacular speakers are excluded from claims of authentic linguistic life unless they also participate in the liturgical chain. Resistance is moderate (0.58) because the alternative readings mount real and organized opposition (the nation-state project, academic linguistics communities, everyday Hebrew speakers), but the institutional authority defending this reading has deep structural backing. The measurements show a slow extraction accumulation over the interval (extractiveness rising 0.38→0.62), which models the gradual intensification of competing claims and the institutional need to suppress them more actively. Theater rises initially then plateaus, suggesting the theatrical maintenance function stabilizes once the reading's boundaries are established. Suppression requirement rises steadily, modeling increasing external pressure from the alternative readings that must be actively countered.
 *
 * PERSPECTIVAL GAP:
 *   From the liturgical authority seat: this is a genuine coordination function (preserving a sacred language, maintaining Jewish continuity through diaspora and persecution, preventing corruption of authoritative texts). The measured extraction is the cost of maintaining that function against modernization pressure — necessary enforcement, not parasitic overhead. From the modern Hebrew speaker seat: this is suppression and extraction — the gate on authenticity is used to delegitimize their language and reinforce institutional power, and the 'continuous chain' framing masks real discontinuities and reconstructions. From the competing reading seats (native_generational_reading, marketplace_pidgin_reading): this reading forecloses their aliveness criteria and thus declares their linguistic projects invalid or inauthentic. The engine computes these divergences per seat from the structural data; the authored claim (tangled_rope) reflects the reading's own self-understanding (genuine coordination + active enforcement), while the metrics reflect the structural effects (gatekeeping, suppression of alternatives, extraction of institutional power). The perspectival gap is precisely what the kernel contest exploits: one reading's coordination is another reading's extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The liturgical_authority_structure is a beneficiary in the full sense: it controls the chain, sets its standards, adjudicates authenticity, and collects institutional prestige and power from gatekeeping the definition. Directionality near the beneficiary end (d~0.15). Jewish institutional continuity as an abstract beneficiary also sits near the beneficiary end because the reading provides one pathway (perhaps the only guaranteed pathway under diaspora conditions) for linguistic and ethnic continuity. The sacred_hebrew_tradition appears as a victim because the reading's defense of it against modernization is also a defense that confines it, makes it inaccessible, and creates pressure for modern Hebrew to develop separately — the tradition's aliveness claim requires its suppression from everyday life. The hebrew_vernacular_speakers and ben_yehuda project heirs are direct targets of extraction: their language is delegitimized, their everyday speech is reframed as imposture or secular construction, their claim to linguistic authenticity is suppressed. Directionality near the target end (d~0.85). Modern Israeli speakers occupy an ambiguous middle: they use Hebrew for all functions, but under this reading they are participants in a constructed language rather than a living one. With no exit option into authentic Hebrew (by this reading's logic, authentic Hebrew is locked into the liturgical chain), they are trapped. Directionality near target end (d~0.80). The beneficiary's directionality is unusually asymmetric relative to the targets because the reading concentrates authority and power in institutional hands while diffusing exclusion across millions of speakers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preserving Hebrew linguistic and cultural continuity through diaspora and dispersion, preventing the language from becoming fossilized in dead texts while ensuring doctrinal continuity — is LIVE and serious. The reading addresses it directly. However, the mandate has been substantially transformed by Ben-Yehuda's project and modern Israel's existence. The problem the reading was built to solve (how to maintain Hebrew aliveness without a political state or geographic center) has been partly superseded by the existence of Modern Hebrew as a functioning vernacular and a nation-state with Hebrew as official language. The reading's response is to redraw the boundary: authentic aliveness is not vernacular aliveness but liturgical-chain aliveness, and the modern state project represents desecration of that mandate, not fulfillment. This redefinition allows the reading to treat its founding problem as still live (the sacred tradition must be continuously transmitted and not corrupted) while confronting a fundamentally changed world. The analysis prevents false-positive mandatrophy: the reading is not a zombie institution — it has actively adapted its mandate in response to changed conditions. But the mandate shift is itself extractive for those outside the liturgical chain, and that is captured in the moderate-high extractiveness and suppression metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_native_speaker_definition,
    'Is a language genuinely alive when sacred texts are preserved in liturgical chain but NO native speakers use it for daily life, or is the lack of vernacular speakers a linguistic death masked by ritual continuity?',
    'Examine whether liturgical Hebrew speakers can conduct ordinary conversation, negotiate novel ideas, or use the language outside liturgical contexts without returning to translation or code-switching. Compare with attested dead languages whose liturgical traditions persist (Latin, Classical Arabic in Quranic recitation).',
    'If liturgical fluency without vernacular use is accepted as ''alive,'' the reading holds and native-speaker literacy is a separate constraint entirely. If vernacular competence is required for true aliveness, the reading becomes a doctrine of linguistic preservation-as-theater, not linguistic life.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_vs_native_speaker_definition, conceptual, 'Whether absence of vernacular speakers disqualifies a language from being called alive if its liturgical transmission is unbroken.').

omega_variable(
    ben_yehuda_project_as_desecration_or_resurrection,
    'Was Ben-Yehuda''s revival project (late 19th century onward) a resurrection of a dead language, or a desecration and replacement of a liturgically-preserved sacred language with a constructed vernacular impostor?',
    'Historical linguistics analysis of the morphology, syntax, and lexicon of Ben-Yehuda''s constructed Modern Hebrew vs. Medieval Rabbinic and Biblical Hebrew. Testimony from contemporary liturgical authorities on whether they experienced the modern language as a continuation or rupture of the tradition.',
    'If desecration framing is correct, the Modern Hebrew state project is victim in a snare (constructed language replacing the constraint''s beneficiary). If resurrection framing is correct, modern vernacular extends the same linguistic life into a new domain. This divergence is the kernel contest itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ben_yehuda_project_as_desecration_or_resurrection, empirical, 'Whether the modern Hebrew language project continues or supplants the liturgical preservation chain.').

omega_variable(
    sacred_tradition_as_victim_or_beneficiary,
    'Does the liturgical preservation reading protect the sacred tradition from dilution, or does enforcement of the reading extract from the tradition by rendering it inaccessible to ordinary speakers and confining it to specialists?',
    'Survey of theological and rabbinic voices across the interval: who attests that liturgical gatekeeping protects sacred meaning, and who attests that it reduces accessibility and damages transmission to new generations?',
    'If tradition-protection framing is correct, the constraint has no victim (it defends what it names). If accessibility-extraction framing is correct, sacred tradition itself is a victim of the specialization and gatekeeping the reading requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_tradition_as_victim_or_beneficiary, preference, 'Whether liturgical preservation protects or extracts from the sacred tradition itself.').

omega_variable(
    kernel_reading_contest_frame,
    'This constraint is ONE reading of the ''hebrew_linguistic_life'' kernel. Three irreconcilable readings exist: this reading (liturgical_preservation_reading), the marketplace reading (pidgin_reading), and the native-generational reading. Do all three remain live interpretations of a single coherent claim, or does the contest decompose into genuinely distinct claims?',
    'Attempt to construct a unified definition of ''linguistic aliveness'' that accommodates all three readings simultaneously. Where unification fails, the kernel is under-specified or contested such that enforcement of one reading forecloses the others.',
    'If all three readings can coexist (different parties holding different readings, each valid for their context), the kernel is genuinely contested and none forecloses the others. If one reading''s core premise logically rules out another''s, the foreclosure relation is structural and the readings occupy irreconcilable logical positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_frame, conceptual, 'Whether the three readings of the hebrew_linguistic_life kernel are genuinely coexisting or logically foreclosed by each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t5, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 5, 0.36).
narrative_ontology:measurement_basis(hebr_tr_t5, observed).
narrative_ontology:measurement(hebr_tr_t10, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(hebr_tr_t10, observed).
narrative_ontology:measurement(hebr_tr_t15, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(hebr_tr_t15, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t25, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(hebr_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t5, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(hebr_be_t5, observed).
narrative_ontology:measurement(hebr_be_t10, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(hebr_be_t10, observed).
narrative_ontology:measurement(hebr_be_t15, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(hebr_be_t15, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t25, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(hebr_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t5, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(hebr_su_t5, observed).
narrative_ontology:measurement(hebr_su_t10, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(hebr_su_t10, observed).
narrative_ontology:measurement(hebr_su_t15, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(hebr_su_t15, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(hebr_su_t20, observed).
narrative_ontology:measurement(hebr_su_t25, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hebr_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% The hebrew_linguistic_life kernel is contested across three irreconcilable readings: liturgical_preservation_reading (this story — linguistic life = unbroken chain of sacred transmission), native_generational_reading (linguistic life = mother-tongue acquisition and daily vernacular use), and marketplace_pidgin_reading (linguistic life = functional use as inter-communal coordination medium). Each reading is instantiated as a separate constraint story with its own ε, beneficiary/victim structure, and terminal classification. The three stories are linked via network.affects_constraints to enable cross-constraint analysis of reading competition. No unified story can accommodate all three readings; attempting to do so would violate ε-invariance (the metric scores and victim sets differ substantially across readings). Each reading is written clean, as structurally true from that reading's own epistemic premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
