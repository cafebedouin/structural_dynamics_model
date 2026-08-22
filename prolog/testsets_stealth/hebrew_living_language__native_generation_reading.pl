% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__native_generation_reading, []).

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
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Native-Generation Criterion of Hebrew Vitality (Revival Enforcement Reading)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This file instantiates the native_generation_reading of the contested
 *   kernel hebrew_living_language: the claim that Hebrew becomes a living
 *   language only when native speakers produce daily speech generatively, not
 *   via memorized recitation. Historically, this criterion was the organizing
 *   target of the revival from 1881 onward — it defined what success meant,
 *   funded ulpanim and Hebrew-only schooling, mandated Hebrew households, and
 *   thereby licensed the displacement of Yiddish, Ladino, and the
 *   Judeo-Arabic dialects from public space and then from the home. The
 *   arrangement fuses a genuine coordination achievement (a shared spoken
 *   medium for immigrants of dozens of mother tongues, and ultimately a
 *   native-speaking nation) with asymmetric extraction (existing vernacular
 *   communities bore the displacement, with no compensation and shrinking
 *   room to refuse). The claim/metric independence rule is honored:
 *   claimed_type is authored from the structure (both coordination and
 *   extraction, actively enforced), while the metrics describe the actual
 *   operation — including the post-1948 enforcement crescendo against Mizrahi
 *   vernaculars. Sibling readings are separate files linked via
 *   network.affects_constraints; per Rule 1, no hedge across readings appears
 *   here.
 *
 * KEY AGENTS:
 *   - - zionist_national_institutions: Agenda setter (institutional/arbitrage) — defines the criterion, funds and staffs the enforcement machinery, collects legitimacy from its operation
 *   - - hebrew_speaking_yishuv: Primary beneficiary (organized/identity_locked) — collects the shared spoken language; dual-positioned, also bears Hebraization costs
 *   - - yiddish_speakers: Primary target (organized/constrained) — largest displaced vernacular, with real but ultimately defeated resistance capacity
 *   - - ladino_speakers: Secondary target (moderate/trapped) — Sephardi vernacular with thin institutional defense
 *   - - judeo_arabic_speakers: Tertiary target (powerless/trapped) — post-1948 mass Hebraization under compulsory schooling and transit-camp conditions
 *   - - diaspora_yiddishist_intelligentsia: Excluded voice (organized/mobile) — contests the criterion itself from outside the decision-making table
 *   - - israeli_education_establishment: Enforcement arm (institutional/constrained) — executes compulsory Hebraization after 1948
 *   - - sociolinguistic_observers: Analytical observer (analytical/analytical) — sees the full structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.58).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.6).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Native-Generation Criterion of Hebrew Vitality (Revival Enforcement Reading)").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '54479fcf-d153-4651-825e-38ef2dbd8a2b').
narrative_ontology:cs_kernel_codification('54479fcf-d153-4651-825e-38ef2dbd8a2b', distributed).
narrative_ontology:cs_authority_grounding('54479fcf-d153-4651-825e-38ef2dbd8a2b', distributed).
narrative_ontology:cs_reading_relation('54479fcf-d153-4651-825e-38ef2dbd8a2b', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('54479fcf-d153-4651-825e-38ef2dbd8a2b', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('54479fcf-d153-4651-825e-38ef2dbd8a2b', foundational, vitality_requires_native_generative_production).
narrative_ontology:cs_axiom_status(vitality_requires_native_generative_production, holdable).
narrative_ontology:cs_axiom_grounding('54479fcf-d153-4651-825e-38ef2dbd8a2b', vitality_requires_native_generative_production, empirically_contingent).
narrative_ontology:cs_axiom('54479fcf-d153-4651-825e-38ef2dbd8a2b', foundational, reconstruction_justifies_vernacular_displacement).
narrative_ontology:cs_axiom_status(reconstruction_justifies_vernacular_displacement, holdable).
narrative_ontology:cs_axiom_grounding('54479fcf-d153-4651-825e-38ef2dbd8a2b', reconstruction_justifies_vernacular_displacement, instrumental).
narrative_ontology:cs_reference_frame('54479fcf-d153-4651-825e-38ef2dbd8a2b', hebrew_as_reborn_daily_vernacular).
narrative_ontology:cs_drift_state('54479fcf-d153-4651-825e-38ef2dbd8a2b', contemporary_sociolinguistic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('54479fcf-d153-4651-825e-38ef2dbd8a2b', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_speaking_yishuv).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, zionist_national_institutions).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, judeo_arabic_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, hebrew_speaking_yishuv).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the movement's linguistic goal — a Hebrew spoken natively in daily life — and fund the machinery that pursues it: Hebrew-only schools, ulpanim, a Hebrew press, language committees, and later state academies. They campaign against Yiddish and German-language schooling in the Yishuv, police public language choice, and after 1948 extend compulsory Hebrew instruction to every immigrant child. Their authority compounds with each institution that comes to operate in Hebrew; abandoning the strict criterion would have cost them the organizing purpose of a vast apparatus, while reframing it (as they eventually did, toward enrichment and purity rather than survival) cost little.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_national_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).

% Immigrants and their children who adopt Hebrew as sole public and eventually home language. They gain a shared spoken medium that lets Poles, Russians, Yemenites, and Germans talk to one another, and their children grow up native speakers — the arrangement's headline product. They pay by discarding mother tongues, by the labor of adult language learning, and by forfeiting the literature, humor, and intimacy of their vernaculars. Leaving Hebrew means stepping outside the national project they crossed oceans to build, which almost none can contemplate; the language and the collective purpose have fused into a single identity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_speaking_yishuv, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, hebrew_speaking_yishuv, payer).

% Speakers of the largest Jewish vernacular, served by a dense press, theater world, party system, and literature. In the Yishuv their language is stigmatized as diaspora 'jargon,' their schools denied funding, their theaters harassed, their newspapers marginalized; parents watch grandchildren grow unable to speak with them. Refusal carries job, schooling, and marriage-market costs; compliance means the language thins generation by generation. Their resistance — the War of the Languages, the surviving Yiddish press — was real and ultimately defeated, a defeat sealed by the destruction of the European centers that anchored their strength.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    organized, biographical, constrained, continental).

% Sephardi communities whose Judeo-Spanish vernacular carried five centuries of post-expulsion life across the Balkans, Turkey, and the Levant. Smaller in numbers and thinner in Yishuv institutional muscle than the Yiddish-speaking majority, they lack the resources to mount comparable defense; the language retreats from the street to the home and then to memory within two generations, with no compensating institutional space permitted for its maintenance.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    moderate, biographical, trapped, regional).

% Mizrahi Jews from Yemen, Iraq, North Africa, and Central Asia whose everyday speech is Judeo-Arabic and kindred dialects. After 1948 they arrive in mass waves, are settled in transit camps, and their children are schooled exclusively in Hebrew; adults need Hebrew for work, rationing, and every bureaucratic interface. Within a single generation the dialects of millennia are largely abandoned. They had no seat where language policy was set and no realistic refusal: employment, housing, and their children's advancement all ran through Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, judeo_arabic_speakers, payer,
    powerless, biographical, trapped, national).

% Writers, scholars, and activists centered in Eastern Europe and later America who hold that Yiddish — not revived Hebrew — is the living language of the Jewish people. They publish sustained critiques of the Hebrew-only regime and of the criterion of vitality behind it, arguing that a language of daily life already existed and needed defending, not replacing. They sit outside the Yishuv's decision-making bodies entirely; their objection registers as polemic across an ocean, not as a voice in the room where the regime was designed.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, diaspora_yiddishist_intelligentsia, excluded,
    organized, generational, mobile, continental).

% The state school system, army Hebrew instruction, and absorption services that after 1948 make Hebrew teaching universal and compulsory. Teachers correct or penalize vernacular use in classrooms; ulpan attendance is tied to employment and settlement benefits; immigrant children are sometimes enlisted as translators and enforcers at home. They execute policy set above them, depend on the ministry for their positions, and bear little of the cultural cost of the policy they administer.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, israeli_education_establishment, agenda_setter,
    institutional, generational, constrained, national).

% Linguists and historians who document the revival, measure intergenerational transmission, date the first self-sustaining native cohorts, and record the fate of the displaced vernaculars. They can compare the replacement outcome against counterfactual multilingual equilibria and against other revitalization attempts worldwide. They collect nothing from the arrangement and bear none of its costs; their assessments carry weight only insofar as the contending parties find them useful.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, hebrew_speaking_yishuv).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of forging a mutually intelligible spoken medium among immigrants from dozens of language backgrounds and anchoring national renewal in a daily language: the native-generation criterion defines the target state around which teaching, household practice, school policy, and institution-building all organize.
% TRANSFER_FUNCTION: Moves linguistic allegiance, public space, institutional access, and intergenerational transmission from the existing Jewish vernaculars (Yiddish, Ladino, Judeo-Arabic) to Hebrew; moves cultural capital and belonging to those who adopt Hebrew and away from those who retain the vernaculars, uncompensated.
% ABSENT_VOICES: Diaspora Yiddishist intelligentsia would contest the criterion itself and point to Yiddish as the actually-living Jewish language; Ladino and Judeo-Arabic cultural figures had no seat at all when Yishuv language policy was fixed; Mizrahi immigrants encountered a settled regime on arrival. Their objections survive in polemic, press archives, and oral history rather than in the deliberations that produced the policy — unanimity in the founding record reflects who was in the room.
% DISAPPEARANCE_RATIONALE: If the native-generation criterion and its enforcement vanished overnight, the revival loses its organizing target: ulpanim, Hebrew-only schooling, and household Hebraization lose their justification, Hebrew likely remains what it was before 1881 — a liturgical and scholarly language — and the Jewish sphere stays multilingual, with Yiddish anchored in Eastern Europe and America, Ladino in the Balkans and Levant, and Judeo-Arabic across Muslim lands. A nation's daily speech, school system, and identity anchor all hang on this arrangement, so the world rearranges.
% FOUNDING_PROBLEM: A people scattered across many vernaculars, possessing a sacred language that no one spoke natively, needed a shared spoken medium and a linguistic anchor for national renewal; the founding problem was how to make Hebrew answer daily needs again — which, under this reading, required manufacturing native generative speakers rather than continuing recitation or literature.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: sociolinguistic historiography of the first native cohorts and the revival's demographics attests both the reality of the founding problem and its closure by the 1920s-30s; Yiddishist and Ladino cultural archives independently confirm that the absence of a shared spoken Jewish medium was real and that the displaced communities experienced the solution as their loss. No party outside the Hebrew-speaking collective attests that the founding problem remains live; the institutions that once pursued it now pursue enrichment and purity, which is a different problem.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_living_language__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the displacement of three major vernaculars is a real, uncompensated transfer of linguistic allegiance and cultural capital, yet it rode on and delivered a broadly shared good. Suppression (0.60) is authored as a raw structural property — it is NOT scaled by power or scope; only extractiveness is scaled by the engine — reflecting school penalties, funding denial, social sanction, and later compulsory state instruction. Theater ratio (0.32) is low-to-moderate: the teaching and speaking were overwhelmingly functional, but Academy purism and public campaigns against 'errors' grew increasingly performative as the outcome became secure, and rulings were widely ignored in practice. Accessibility collapse (0.50): alternatives were half-collapsed — vernaculars survived privately and in diaspora but were excluded from schools, workplaces, and public institutions, so exit existed only at the price of cultural severance. Resistance (0.60): the War of the Languages (1913-14), the persistent Yiddish press, Yiddishist polemic, and the sheer demographic weight of the vernaculars constituted sustained resistance; a coalition of the three victim groups was conceivable but never formed — the Holocaust destroyed Yiddish's center of gravity before coalition politics could mature, and Ladino and Judeo-Arabic communities lacked the organized base. The temporal series run on one shared grid (t=0..80 mapping approximately 1881-1961: 0=1881 arrival of Ben-Yehuda, 16=1897 First Zionist Congress era, 32=1913 War of the Languages, 48=1929 entrenched Hebrew-only Yishuv, 64=1945-49 enforcement crescendo, 80=1961 consolidation). Extraction accumulates to a 1945-49 peak (0.62) then relaxes slightly (0.58) once victory is secured; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity change — machinery built up from volunteer committees to compulsory state schooling, then partially stood down — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute differently: from the Zionist institutions the criterion is the movement's crowning engineering success, self-evidently worth its costs; from the vernacular seats the same structure is enforced erasure of inherited worlds. There is also a sharp same-level lateral divergence: yiddish_speakers and hebrew_speaking_yishuv both hold 'organized' power, yet stand on opposite sides of the enforcement line — differentiated not by global standing but by constraint-specific exit asymmetry. The Hebrew speaker's exit means abandoning the national project they crossed oceans to build (identity-locked); the Yiddish speaker's exit means cultural loss but remained materially possible (constrained). Inter-institutionally, the pre-state voluntary bodies, the post-1948 state education establishment, and the external diaspora Yiddishist organizations experienced the identical criterion as mission, mandate, and menace respectively. Identity-lock dynamics: the yishuv's fusion is national-professional — the revival WAS the collective enterprise, so questioning the criterion read as betrayal; had that frame broken (as it partially has in contemporary multilingual Israel), the enforcement edge would lose its volunteers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation and no overrides are needed. zionist_national_institutions (agenda_setter, institutional, arbitrage-grade exit) derive near the beneficiary end — they wrote the rules and could reframe them. hebrew_speaking_yishuv (beneficiary with secondary payer role, identity_locked) derives net-beneficiary but not subsidized: they collected the shared language and paid in discarded mother tongues and adult learning labor, pulling d up from the pure-beneficiary floor. yiddish_speakers (payer, organized, constrained), ladino_speakers (payer, moderate, trapped), and judeo_arabic_speakers (payer, powerless, trapped) derive progressively nearer the full-target end as exit narrows and power falls — the trapped, powerless Mizrahi immigrants sit closest to full extraction, which is where the post-1948 enforcement concentrated. Scope is regional-to-national; the engine's scope amplification applies modestly to the verification-hardened national phase.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — create a self-sustaining population producing Hebrew natively — was substantially closed by the 1920s-30s, when child-to-child Hebrew transmission stabilized. The criterion and its enforcement apparatus persisted long after, redirecting toward boundary maintenance: purity campaigns, stigmatization of residual vernacular use, and policing of public language choice. Mandatrophy is therefore declared resolved (base_properties.mandatrophy_resolved: true), and the R5 interview records founding_problem_status: dead against disappearance_verdict: world_rearranges — the honest mismatch that flags persistence-after-success. The classification prevents mislabeling in both directions: the real, broad-based coordination achievement blocks a pure-extraction reading (this was not a protection racket with a grammar book), while the asymmetric, uncompensated displacement blocks a pure-coordination reading (the victims were not merely paying fair dues). Theatricality is treated as symptom, not test: the cost-asymmetry test shows today's administrators could relax purity enforcement cheaply, no party profits enough to maintain the strict-exclusivist edge, and no party hurts enough to dismantle it — the residual regime drifts inertial at the margins even as the language itself thrives. Language health and constraint function must not be conflated: the vernacular is vibrantly alive; the criterion-as-enforcement is the part that outlived its engineering purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monosemy_of_living_language,
    'Is ''living language'' a single property governed by one criterion (making this reading''s exclusivity binding and its foreclosure of the liturgical sibling final), or a polysemous term admitting distinct senses (vernacular, liturgical, literary vitality) under which the three readings coexist as complementary operationalizations?',
    'Examine whether comparative vitality frameworks (EGIDS-scale assessments, UNESCO language-vitality factors) are adopted by the disputing communities as rival or as complementary measures, and whether any party concedes the siblings track something real.',
    'If polysemous, the forecloses relation to liturgical_continuity_reading softens to coexists_with and this reading loses its exclusivist force; the epsilon and victim structure of this file are unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monosemy_of_living_language, conceptual, 'Whether the kernel term has one criterion (exclusive readings) or several senses (coexisting readings).').

omega_variable(
    suppression_structural_vs_internalized,
    'Was the abandonment of Yiddish, Ladino, and Judeo-Arabic driven by structural enforcement (school penalties, job gatekeeping, transit-camp conditions, denial of institutional funding) or by internalized stigma (shame toward ''jargon,'' prestige aspiration, parental choice to spare children disadvantage)?',
    'Post-enforcement trajectory: if vernacular use rebounded once coercion relaxed (1960s onward), suppression was structural; the observed partial non-rebound suggests internalized stigma persisted after the barriers fell.',
    'If substantially internalized, effective suppression exceeds the structural measure — the displaced communities carry the suppression with them after exit, and the victim set''s costs outlast the enforcement regime that produced them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of vernacular abandonment.').

omega_variable(
    counterfactual_multilingual_coexistence,
    'Could a shared spoken Hebrew have been achieved without suppressing the existing vernaculars — Hebrew as added lingua franca in a stable multilingual equilibrium rather than a replacement tongue?',
    'Comparative revitalization cases (Irish, Norwegian standardization, Swiss multilingual stability) and diaspora circles where Hebrew spread without coercion; test whether native-generative transmission can stabilize alongside robust vernacular maintenance.',
    'If separable, the extraction component is attributable to the exclusivist enforcement overlay rather than the criterion itself, supporting the hybrid rather than pure-extraction classification; if inseparable, part of the measured extraction is the price of the coordination achievement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_multilingual_coexistence, conceptual, 'Whether the coordination function and the vernacular-displacement cost are structurally separable.').

omega_variable(
    reading_indexicality_of_vitality_claim,
    'This file is one reading of the kernel hebrew_living_language: under the liturgical sibling the claim ''Hebrew is living'' is continuously true across the diaspora centuries, while under this reading it is false until roughly the 1920s-30s — is the disagreement located in the definition of vitality (conceptual) or in the empirical question of what sustains intergenerational transmission?',
    'Per the epsilon-invariance decomposition rule, each reading authors its own story with its own epsilon and victim set (done here); the residual question is whether any metric neutral across readings can adjudicate, or whether the contest is irreducibly indexical to the reading adopted.',
    'Confirms the family must remain three linked files rather than one constraint measured three ways; fixes that this file''s epsilon refers to the revival-enforcement arrangement, not to the liturgical or literary arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_vitality_claim, conceptual, 'Location of the kernel contest: definition versus empirical mechanism.').

omega_variable(
    native_generation_onset_date,
    'When did native generative transmission become self-sustaining — marking closure of the founding problem this criterion was built to solve?',
    'Demographic and educational records of the first Hebrew-native cohorts (agricultural settlements of the 1890s-1900s, Yishuv-wide stabilization by the 1920s-30s), cross-checked against contemporaneous testimony that child-to-child Hebrew play had become unremarkable.',
    'Dates mandatrophy onset precisely; an earlier closure date strengthens the dead founding-problem status and the persistence-after-success reading of the subsequent enforcement record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_generation_onset_date, empirical, 'Closure date of the founding engineering problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__native_generation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hebr_tr_t16, hebrew_living_language__native_generation_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(hebr_tr_t32, hebrew_living_language__native_generation_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(hebr_tr_t48, hebrew_living_language__native_generation_reading, theater_ratio, 48, 0.22).
narrative_ontology:measurement(hebr_tr_t64, hebrew_living_language__native_generation_reading, theater_ratio, 64, 0.28).
narrative_ontology:measurement(hebr_tr_t80, hebrew_living_language__native_generation_reading, theater_ratio, 80, 0.32).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__native_generation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hebr_be_t16, hebrew_living_language__native_generation_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(hebr_be_t32, hebrew_living_language__native_generation_reading, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(hebr_be_t48, hebrew_living_language__native_generation_reading, base_extractiveness, 48, 0.52).
narrative_ontology:measurement(hebr_be_t64, hebrew_living_language__native_generation_reading, base_extractiveness, 64, 0.62).
narrative_ontology:measurement(hebr_be_t80, hebrew_living_language__native_generation_reading, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__native_generation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hebr_su_t16, hebrew_living_language__native_generation_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(hebr_su_t32, hebrew_living_language__native_generation_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(hebr_su_t48, hebrew_living_language__native_generation_reading, suppression_requirement, 48, 0.5).
narrative_ontology:measurement(hebr_su_t64, hebrew_living_language__native_generation_reading, suppression_requirement, 64, 0.66).
narrative_ontology:measurement(hebr_su_t80, hebrew_living_language__native_generation_reading, suppression_requirement, 80, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'Hebrew is a living language' conflates three structurally distinct claims. liturgical_continuity_reading authors the recitation-continuity claim (negligible extraction, no victim set, continuous truth across the diaspora). literary_revival_reading authors the Haskalah-literacy claim (low extraction, contested reachability, upstream supplier of the modernized lexicon this reading consumed). This file authors the native-generation claim (moderate extraction, victims = displaced vernacular speakers, enforcement-dependent, strict-reachability break acknowledged — the criterion was unreachable from continuity alone, so deliberate reconstruction was required). Upstream/downstream structure: literary competence fed this reading's project; this reading's success retroactively changed the literary reading's legitimacy conditions (sufficient -> preparatory). All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
