% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: WP:N Notability Gate — Inclusionist Reading
 *   domain: social/knowledge_infrastructure/platform_governance
 *
 * SUMMARY:
 *   English Wikipedia's notability policy (WP:N), together with the deletion
 *   machinery that enforces it, decides which subjects exist in the world's
 *   largest reference work. This file instantiates the inclusionist reading
 *   of the notability_guidelines kernel: the operated criteria set — the
 *   general notability guideline, the topic-specific supplements, the
 *   reliable-source hierarchy, and the articles-for-deletion process —
 *   constitutes a credentialing boundary that converts the shape of the
 *   published institutional record into the boundary of encyclopedic reality.
 *   Topics documented chiefly in oral transmission, non-indexed local press,
 *   non-Anglophone scholarship, or community memory fail the test in bulk,
 *   and the unpaid labor invested in them is destroyed at scale; the
 *   institutions whose publications serve as admissible evidence acquire
 *   definitional authority over the encyclopedia's universe without entering
 *   its forums. The ε referent is the standing arrangement under contest —
 *   WP:N as actually operated on English Wikipedia, assessed by this
 *   reading's own lights — never the source-pluralist arrangement this
 *   reading would install. Claim and metrics are authored independently: this
 *   seat claims snare; the authored metrics describe high-extraction,
 *   actively-enforced, slowly-hardening operation. Sibling readings
 *   (deletionist, deliberative) are separate constraints linked in the
 *   network section; the committer structure is routed to omegas per the
 *   kernel protocol. KEY AGENTS (by structural relationship): -
 *   indigenous_oral_tradition_communities: Primary target (powerless/trapped)
 *   — knowledge cannot wear citable form; bears erasure -
 *   marginalized_field_practitioners: Primary target (powerless/trapped) —
 *   embodied knowledge below the record - global_south_local_press: Secondary
 *   target (moderate/constrained) — coverage discounted at the evidence gate
 *   - non_anglophone_scholars: Secondary target (moderate/constrained) —
 *   citation networks invisible to verification routines -
 *   inclusionist_editors: Internal target (organized/identity_locked) —
 *   deleted labor, fused with mission - legacy_news_organizations: Primary
 *   beneficiary (institutional/arbitrage) — owns the admission currency -
 *   commercial_academic_publishers: Beneficiary (institutional/arbitrage) -
 *   western_glam_institutions: Beneficiary (institutional/arbitrage) -
 *   wikimedia_foundation: Institutional beneficiary (institutional/arbitrage)
 *   — collects reputational capital - wikipedia_admin_community: Agenda
 *   setter (institutional/identity_locked) — writes and enforces the boundary
 *   - rival_knowledge_commons: Excluded alternative (moderate/mobile) -
 *   media_gap_researchers: Analytical observer (moderate/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.78).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.6).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "WP:N Notability Gate — Inclusionist Reading").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "social/knowledge_infrastructure/platform_governance").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '973e63f9-8505-4f0e-a688-6977f25d104a').
narrative_ontology:cs_kernel_codification('973e63f9-8505-4f0e-a688-6977f25d104a', formalized).
narrative_ontology:cs_authority_grounding('973e63f9-8505-4f0e-a688-6977f25d104a', lineage).
narrative_ontology:cs_interpretation_layer_present('973e63f9-8505-4f0e-a688-6977f25d104a').
narrative_ontology:cs_reading_relation('973e63f9-8505-4f0e-a688-6977f25d104a', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('973e63f9-8505-4f0e-a688-6977f25d104a', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('973e63f9-8505-4f0e-a688-6977f25d104a', foundational, knowledge_legitimacy_requires_source_pluralism).
narrative_ontology:cs_axiom_status(knowledge_legitimacy_requires_source_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('973e63f9-8505-4f0e-a688-6977f25d104a', knowledge_legitimacy_requires_source_pluralism, deontological).
narrative_ontology:cs_axiom('973e63f9-8505-4f0e-a688-6977f25d104a', secondary, procedural_neutrality_vitiated_by_skewed_inputs).
narrative_ontology:cs_axiom_status(procedural_neutrality_vitiated_by_skewed_inputs, holdable).
narrative_ontology:cs_axiom_grounding('973e63f9-8505-4f0e-a688-6977f25d104a', procedural_neutrality_vitiated_by_skewed_inputs, empirically_contingent).
narrative_ontology:cs_reference_frame('973e63f9-8505-4f0e-a688-6977f25d104a', universal_access_epistemic_promise).
narrative_ontology:cs_drift_state('973e63f9-8505-4f0e-a688-6977f25d104a', contemporary_knowledge_equity_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('973e63f9-8505-4f0e-a688-6977f25d104a', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, legacy_news_organizations).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, commercial_academic_publishers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, western_glam_institutions).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, wikimedia_foundation).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, indigenous_oral_tradition_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, global_south_local_press).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_field_practitioners).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, non_anglophone_scholars).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, inclusionist_editors).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, published_record_verifiability_doctrine).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, institutional_credentialed_source_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Veteran volunteer editors who write and interpret the notability policy, close deletion discussions, and enforce compliance through speedy-deletion tagging, draftification, and blocking. Most have spent years building standing inside the project; leaving would mean abandoning accumulated reputation and a stewardship role many describe as central to their lives. They describe their work as applying neutral quality standards, and the subjects most often deleted rarely appear to contest their decisions.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_admin_community, agenda_setter,
    institutional, generational, identity_locked, global).

% Large news organizations whose feature stories, profiles, and event coverage constitute the significant-coverage-in-reliable-sources evidence that admits a topic to the encyclopedia. Their editorial choices about what merits coverage effectively set the encyclopedia's topical universe, yet they take no part in Wikipedia's policy forums and bear none of its editing labor. Referral traffic, citation, and the canonical authority of being the source the encyclopedia relies on flow back to them.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, legacy_news_organizations, beneficiary,
    institutional, generational, arbitrage, global).

% Journal and monograph publishers whose peer-reviewed outputs rank as the highest-grade evidence of notability for scholarly topics. Encyclopedia articles citing their catalogs route prestige, library subscription demand, and citation counts toward them. Their paywalls limit who can even verify the evidence their publications provide.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, commercial_academic_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% Established European and North American galleries, libraries, archives, and museums whose catalogs, exhibition records, and curatorial publications serve as authoritative sourcing for cultural topics. Partnership programs with the encyclopedia raise their visibility. Institutions holding non-Western collections seldom publish in the indexed, English-friendly formats the evidence bar rewards.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, western_glam_institutions, beneficiary,
    institutional, generational, arbitrage, continental).

% The nonprofit that hosts and legally stewards the project. It does not set content policy, but it solicits donations and builds institutional partnerships on the strength of the encyclopedia's reputation for rigor and neutrality, and its strategy documents now prominently acknowledge coverage gaps. Day-to-day boundary-drawing remains with volunteer editors.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikimedia_foundation, beneficiary,
    institutional, civilizational, arbitrage, global).

% Communities that hold detailed, structured knowledge — genealogies, ecological knowledge, law, history — transmitted orally across generations. The transmission format itself fails the admission test: no newspaper reviewed their knowledge systems, and scholarly citations of living tradition-bearers are rare. Taking part in deletion debates would require adopting the literate, institution-mediated framing their knowledge deliberately does not use; converting it into citable prose generally requires an outside intermediary.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, indigenous_oral_tradition_communities, payer,
    powerless, generational, trapped, regional).

% Newspapers, radio stations, and digital outlets that report daily on the artists, officials, businesses, and events of their regions. Much of their output is not indexed by search engines, not archived, or published in languages that sourcing assessments handle poorly, and citations to them are frequently challenged at the reliable-sources noticeboard. They cannot reformat their reporting into the forms the evidence bar recognizes.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, global_south_local_press, payer,
    moderate, biographical, constrained, regional).

% Traditional healers, vernacular architects, folk musicians, craft guilds, and community organizers whose work is documented mainly in local languages, program flyers, ephemeral broadcasts, or memory. Pages about them are commonly nominated for deletion within days of creation, and the embodied knowledge predates and exceeds anything in the published record.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_field_practitioners, payer,
    powerless, generational, trapped, local).

% Researchers publishing in national journals, regional university presses, and languages other than English. Their work sits below the evidence bar as applied, and migrating to high-prestige Anglophone venues takes years and usually requires reframing research away from locally urgent questions. Their citation networks are poorly covered by the search routines editors use to verify sources.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, non_anglophone_scholars, payer,
    moderate, biographical, constrained, continental).

% Volunteer editors who campaign to create and retain articles on under-covered topics: researching obscure sources, arguing at deletion discussions, organizing themed editing drives. A large share of what they build is later deleted. Their standing, social ties, and sense of purpose are bound up with continued participation, so exiting means forfeiting the community that anchors the work.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, inclusionist_editors, payer,
    organized, biographical, identity_locked, global).

% Operators of alternative repositories — thematic wikis, community archives, federated knowledge bases — where different evidentiary norms apply. They have no standing in the policy forums that draw the encyclopedia's boundaries. Their existence shows other arrangements are technically feasible, but none concentrates comparable audience or authority.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, rival_knowledge_commons, excluded,
    moderate, generational, mobile, global).

% Academic and independent researchers who quantify coverage disparities by gender, geography, and language, and analyze whose sources get accepted. They publish findings that both defenders and critics of the criteria cite, and take no part in deletion decisions.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, media_gap_researchers, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, legacy_news_organizations).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real scarcity-coordination problem: unlimited candidate topics, finite volunteer editorial labor. A shared admission criterion lets thousands of unacquainted editors predict which new pages will survive, focuses cleanup attention, and keeps promotional and vanity content from consuming the commons. It also coordinates the community's self-understanding as an encyclopedia rather than a directory.
% TRANSFER_FUNCTION: Moves epistemic recognition and permanence: confers encyclopedic existence on topics carrying institutional published coverage, and removes it from topics lacking such coverage, together with the unpaid labor invested in the deleted pages. Definitional authority over what counts as known flows to holders of mainstream institutional credentials; attention and referral authority flow from the encyclopedia back to the institutions whose coverage served as the ticket.
% ABSENT_VOICES: The people whose knowledge fails the test are almost never in the room: oral-tradition keepers, readers and reporters of small-language press, and the subjects of contested articles cannot easily enter English-language, policy-fluent, asynchronously-deliberated forums. Rival knowledge-commons builders have no standing in deletion decisions. Within the project, casual editors deterred by early deletion experiences exit quietly, leaving the debate to a shrinking cadre of policy-fluent regulars.
% DISAPPEARANCE_RATIONALE: Overnight removal of the notability criteria would halt mass deletion: draft and stub pages would become keepable, creation rates for Global South and oral-history topics would spike as advocates no longer anticipate deletion, and dispute energy would reorganize around whatever successor boundary emerged. The encyclopedia's composition, its editors' incentive structure, and the referral authority flowing to institutional sources would all rearrange; meanwhile the old vanity-and-spam problem would resurface in weakened form until a replacement rule stabilized.
% FOUNDING_PROBLEM: In 2005–2006 the growing wiki was flooded with vanity biographies, band pages, webcomic promotion, and covert advertising; editors needed a principled line between an encyclopedia and an indiscriminate directory, and the notability guideline crystallized out of the 'Wiki is not paper' debates as that line.
% FOUNDING_PROBLEM_CORROBORATION: Coverage-disparity research published by academics outside the movement, and first-hand region reports gathered through Wikimedia movement-strategy consultations, independently document the exclusionary pattern this reading measures. No party outside the benefiting structure attests that the original vanity-flood problem remains the operative justification for the current stringency — deletionist editors assert its liveness from inside the apparatus — so the two attestations conflict and the status is disputed across seats.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the admission currency — significant coverage in reliable sources — is an asset class owned by non-participating institutions, so the boundary of encyclopedic reality replicates the boundary of institutional attention regardless of epistemic merit, and the unpaid labor invested in failing topics is routinely destroyed. Suppression (0.60) is a raw structural property, unscaled by power or scope: it is carried by enforcement machinery (speedy deletion, AfD closure norms, draftification, blocking) plus a chilling layer in which editors pre-censor their own creation efforts; an omega splits the structural and internalized shares. Theater (0.33) reflects deliberation that does real work in easy cases while a growing share of closure outcomes tracks source prestige rather than argument quality — performed neutrality over a skewed input distribution. Accessibility collapse (0.62): alternatives collapse substantially once learned — userified drafts age into deletion, rival platforms lack audience gravity — though off-wiki publication remains possible, keeping the figure below mountain-like levels. Resistance (0.68) is sustained and organized: themed editing campaigns, reform requests for deletion, and published bias audits. All three tracked series share one six-point grid (2006–2024), every metric authored at every point; trajectories are monotonic, so no cyclical analysis applies. Identity-lock dynamics bind two seats: veteran administrators (stewardship identity — the organization has become its function) and inclusionist editors (mission identity — exit means abandoning the community that anchors the work); breaking either frame would change that seat's computed extraction markedly. Coalition check: the payer seats are dispersed across continents and epistemic registers, which historically frustrates coalition formation, but the edit-a-thon network shows class-level coalition capacity is real and rising — reflected in the class-level resistance cell of the coercion grid.
 *
 * PERSPECTIVAL GAP:
 *   From the admin seat the arrangement presents as careful, neutral craftsmanship applied case-by-case; from the beneficiary seats it is invisible — they never enter the forum whose boundaries their products define; from the trapped target seats it presents as an alien credentialing regime whose language their knowledge cannot wear, experienced as erasure rather than evaluation. Same-power divergence: global_south_local_press and non_anglophone_scholars share the moderate power atom with rival_knowledge_commons, yet press and scholars are constrained (their output is fixed in form and place) while platform operators are mobile (they can adopt different norms wholesale). Inter-institutionally, the beneficiary institutions experience no constraint at all — the criteria cost them nothing and return authority — while the administering community experiences cost as conflict and labor, not as extraction. The engine computes per-seat classifications from these structural differences; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats sit at the d≈0 end with arbitrage-grade exit: news organizations, publishers, and GLAM institutions produce their outputs for their own markets and lose little if the criteria changed — the encyclopedia comes to them, not vice versa — so effective extraction damps toward subsidy. The foundation likewise collects reputation without being governed by the criteria. Victim seats sit at the d≈1 end with the worst exit profile in the corpus: trapped by format (oral transmission cannot reformat itself into citability), language, and indexation — the arrangement takes recognition and permanence precisely from those least able to substitute another arena, so effective extraction is amplified to the full-target end. The admin seat is structurally mixed — it expends real labor and absorbs conflict costs while collecting status and mission satisfaction — placing it nearer symmetric than either pole. Inclusionist editors combine payer exposure (deleted labor) with identity-locked exit, amplifying their effective extraction relative to their nominal insider position. No directionality_overrides were authored: the beneficiary/victim declarations plus exit options already fix each seat's relationship, and the available override keys (power atoms) would misfire across the heterogeneous agents sharing each atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the apparatus as pure coordination (rope) would erase the documented asymmetry — the coordination function is real, but the asymmetric transfer rides it and the ride is this reading's point. Reading it as fabricated-from-nothing would erase the genuine founding problem: the vanity flood of 2005–2006 was real, and its residue persists. The mandatrophy lens therefore lands on partial obsolescence: the specific flood that justified crystallization is now largely handled by cheaper adjacent mechanisms (promotional-content rules, spam filtering, notoriety-independent speedy criteria), while the notability boundary itself has hardened into the apparatus's center of gravity — its mandate has drifted from keeping the directory out toward allocating recognition, with no sunset anywhere in sight. Accordingly this story does not declare the mandate resolved; the founding-problem interview records the status as contested, and the R5 mismatch consumer can cross-check that against the computed theater and persistence paths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the snare verdict belong to WP:N-as-operated, or only to the inclusionist construal of it — how would the deletionist and deliberative readings weight the same structural facts?',
    'Generate the sibling readings as separate constraint files over the same referent and interval, then compare per-seat classifications and effective-extraction distributions across the kernel family.',
    'If the siblings compute materially lower effective extraction on the identical referent, the disagreement localizes in the weighting of the coordination function versus the source-hierarchy asymmetry — the kernel contest is about the evidentiary status of ''reliable source'', not about the underlying facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'This constraint is one reading of the notability_guidelines kernel; classification may be reading-indexed.').

omega_variable(
    coordination_extraction_separability,
    'Is the exclusionary effect separable from the anti-spam and labor-focus functions, or does admitting oral-tradition and local-record evidence necessarily reopen the vanity flood?',
    'Compare sister projects and historical windows with looser criteria: measure spam incidence and survival of Global South topics where the bar was relaxed.',
    'If separable, the extraction component is removable through source-pluralism reforms without losing coordination; if inseparable, part of the measured extraction is the price of the coordination itself and the honest classification drifts toward tangled_rope territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    binding_constraint_or_symptom,
    'Is the notability bar actually binding on marginalized-topic coverage, or would the same attention economy reproduce the prestige hierarchy even under lax criteria?',
    'Natural experiments: creation and survival rates for equivalent topics during enforcement lapses or on projects without the bar, controlling for editor demographics.',
    'If the bar is not binding, ε attributed to this arrangement is partly misattributed inherited media bias and the classification softens; if binding, the snare reading stands with higher confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_constraint_or_symptom, empirical, 'Whether the arrangement causes the exclusion or merely transmits it.').

omega_variable(
    source_prestige_vs_accuracy,
    'Do reliable-source acceptances track accuracy and verification practice, or institutional prestige and format, net of accuracy?',
    'Code the reliable-sources noticeboard archive: acceptance and rejection rates by outlet class with accuracy indicators held fixed.',
    'Prestige-driven acceptance confirms the admission currency is an interest-bearing artifact and supports the high-ε reading; accuracy-driven acceptance supports the deletionist instrument framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_prestige_vs_accuracy, empirical, 'Whether the evidence bar tracks verifiability or prestige.').

omega_variable(
    suppression_internalization_split,
    'How much of the measured suppression is structural enforcement versus internalized self-censorship by editors who have learned not to bother creating such articles?',
    'Compare editors'' topic-creation behavior before and after policy relaxations and across projects with different bars; persistent hesitancy after barrier removal indicates internalization.',
    'A large internalized share means effective suppression exceeds the structural measure and would outlast any single reform; classification consequences concentrate in the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized suppression mechanism split.').

omega_variable(
    leveled_coercion_uncertainty,
    'Which coercion-grid cell assignments are least robust — notably organizational-level suppression and structural-level stakes inflation, judged from documentary traces rather than direct observation?',
    'Targeted archival coding of noticeboard demotion records (organizational level) and reform-attempt failure records (structural level).',
    'Material revisions would reshape the gradient picture without changing the headline verdict; the grid is diagnostic for this story, not load-bearing for the snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leveled_coercion_uncertainty, empirical, 'Robustness bounds on the authored level-resolved coercion judgments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 2006, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpn_inclusionist_tr_t2006, notability_guidelines__inclusionist_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement_basis(wpn_inclusionist_tr_t2006, observed).
narrative_ontology:measurement(wpn_inclusionist_tr_t2010, notability_guidelines__inclusionist_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement_basis(wpn_inclusionist_tr_t2010, observed).
narrative_ontology:measurement(wpn_inclusionist_tr_t2014, notability_guidelines__inclusionist_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement_basis(wpn_inclusionist_tr_t2014, observed).
narrative_ontology:measurement(wpn_inclusionist_tr_t2018, notability_guidelines__inclusionist_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement_basis(wpn_inclusionist_tr_t2018, observed).
narrative_ontology:measurement(wpn_inclusionist_tr_t2022, notability_guidelines__inclusionist_reading, theater_ratio, 2022, 0.32).
narrative_ontology:measurement_basis(wpn_inclusionist_tr_t2022, observed).
narrative_ontology:measurement(wpn_inclusionist_tr_t2024, notability_guidelines__inclusionist_reading, theater_ratio, 2024, 0.33).
narrative_ontology:measurement_basis(wpn_inclusionist_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(wpn_inclusionist_be_t2006, notability_guidelines__inclusionist_reading, base_extractiveness, 2006, 0.5).
narrative_ontology:measurement_basis(wpn_inclusionist_be_t2006, observed).
narrative_ontology:measurement(wpn_inclusionist_be_t2010, notability_guidelines__inclusionist_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement_basis(wpn_inclusionist_be_t2010, observed).
narrative_ontology:measurement(wpn_inclusionist_be_t2014, notability_guidelines__inclusionist_reading, base_extractiveness, 2014, 0.65).
narrative_ontology:measurement_basis(wpn_inclusionist_be_t2014, observed).
narrative_ontology:measurement(wpn_inclusionist_be_t2018, notability_guidelines__inclusionist_reading, base_extractiveness, 2018, 0.71).
narrative_ontology:measurement_basis(wpn_inclusionist_be_t2018, observed).
narrative_ontology:measurement(wpn_inclusionist_be_t2022, notability_guidelines__inclusionist_reading, base_extractiveness, 2022, 0.76).
narrative_ontology:measurement_basis(wpn_inclusionist_be_t2022, observed).
narrative_ontology:measurement(wpn_inclusionist_be_t2024, notability_guidelines__inclusionist_reading, base_extractiveness, 2024, 0.78).
narrative_ontology:measurement_basis(wpn_inclusionist_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(wpn_inclusionist_su_t2006, notability_guidelines__inclusionist_reading, suppression_requirement, 2006, 0.4).
narrative_ontology:measurement_basis(wpn_inclusionist_su_t2006, observed).
narrative_ontology:measurement(wpn_inclusionist_su_t2010, notability_guidelines__inclusionist_reading, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement_basis(wpn_inclusionist_su_t2010, observed).
narrative_ontology:measurement(wpn_inclusionist_su_t2014, notability_guidelines__inclusionist_reading, suppression_requirement, 2014, 0.52).
narrative_ontology:measurement_basis(wpn_inclusionist_su_t2014, observed).
narrative_ontology:measurement(wpn_inclusionist_su_t2018, notability_guidelines__inclusionist_reading, suppression_requirement, 2018, 0.56).
narrative_ontology:measurement_basis(wpn_inclusionist_su_t2018, observed).
narrative_ontology:measurement(wpn_inclusionist_su_t2022, notability_guidelines__inclusionist_reading, suppression_requirement, 2022, 0.59).
narrative_ontology:measurement_basis(wpn_inclusionist_su_t2022, observed).
narrative_ontology:measurement(wpn_inclusionist_su_t2024, notability_guidelines__inclusionist_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement_basis(wpn_inclusionist_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=2006, tn=2024
narrative_ontology:measurement(wpn_inclusionist_grid_01, notability_guidelines__inclusionist_reading, accessibility_collapse(class), 2006, 0.4).
narrative_ontology:measurement(wpn_inclusionist_grid_02, notability_guidelines__inclusionist_reading, accessibility_collapse(class), 2024, 0.68).
narrative_ontology:measurement(wpn_inclusionist_grid_03, notability_guidelines__inclusionist_reading, accessibility_collapse(individual), 2006, 0.35).
narrative_ontology:measurement(wpn_inclusionist_grid_04, notability_guidelines__inclusionist_reading, accessibility_collapse(individual), 2024, 0.6).
narrative_ontology:measurement(wpn_inclusionist_grid_05, notability_guidelines__inclusionist_reading, accessibility_collapse(organizational), 2006, 0.3).
narrative_ontology:measurement(wpn_inclusionist_grid_06, notability_guidelines__inclusionist_reading, accessibility_collapse(organizational), 2024, 0.55).
narrative_ontology:measurement(wpn_inclusionist_grid_07, notability_guidelines__inclusionist_reading, accessibility_collapse(structural), 2006, 0.25).
narrative_ontology:measurement(wpn_inclusionist_grid_08, notability_guidelines__inclusionist_reading, accessibility_collapse(structural), 2024, 0.62).
narrative_ontology:measurement(wpn_inclusionist_grid_09, notability_guidelines__inclusionist_reading, resistance(class), 2006, 0.3).
narrative_ontology:measurement(wpn_inclusionist_grid_10, notability_guidelines__inclusionist_reading, resistance(class), 2024, 0.65).
narrative_ontology:measurement(wpn_inclusionist_grid_11, notability_guidelines__inclusionist_reading, resistance(individual), 2006, 0.45).
narrative_ontology:measurement(wpn_inclusionist_grid_12, notability_guidelines__inclusionist_reading, resistance(individual), 2024, 0.5).
narrative_ontology:measurement(wpn_inclusionist_grid_13, notability_guidelines__inclusionist_reading, resistance(organizational), 2006, 0.1).
narrative_ontology:measurement(wpn_inclusionist_grid_14, notability_guidelines__inclusionist_reading, resistance(organizational), 2024, 0.25).
narrative_ontology:measurement(wpn_inclusionist_grid_15, notability_guidelines__inclusionist_reading, resistance(structural), 2006, 0.35).
narrative_ontology:measurement(wpn_inclusionist_grid_16, notability_guidelines__inclusionist_reading, resistance(structural), 2024, 0.6).
narrative_ontology:measurement(wpn_inclusionist_grid_17, notability_guidelines__inclusionist_reading, stakes_inflation(class), 2006, 0.25).
narrative_ontology:measurement(wpn_inclusionist_grid_18, notability_guidelines__inclusionist_reading, stakes_inflation(class), 2024, 0.6).
narrative_ontology:measurement(wpn_inclusionist_grid_19, notability_guidelines__inclusionist_reading, stakes_inflation(individual), 2006, 0.3).
narrative_ontology:measurement(wpn_inclusionist_grid_20, notability_guidelines__inclusionist_reading, stakes_inflation(individual), 2024, 0.55).
narrative_ontology:measurement(wpn_inclusionist_grid_21, notability_guidelines__inclusionist_reading, stakes_inflation(organizational), 2006, 0.15).
narrative_ontology:measurement(wpn_inclusionist_grid_22, notability_guidelines__inclusionist_reading, stakes_inflation(organizational), 2024, 0.4).
narrative_ontology:measurement(wpn_inclusionist_grid_23, notability_guidelines__inclusionist_reading, stakes_inflation(structural), 2006, 0.35).
narrative_ontology:measurement(wpn_inclusionist_grid_24, notability_guidelines__inclusionist_reading, stakes_inflation(structural), 2024, 0.7).
narrative_ontology:measurement(wpn_inclusionist_grid_25, notability_guidelines__inclusionist_reading, suppression(class), 2006, 0.2).
narrative_ontology:measurement(wpn_inclusionist_grid_26, notability_guidelines__inclusionist_reading, suppression(class), 2024, 0.45).
narrative_ontology:measurement(wpn_inclusionist_grid_27, notability_guidelines__inclusionist_reading, suppression(individual), 2006, 0.35).
narrative_ontology:measurement(wpn_inclusionist_grid_28, notability_guidelines__inclusionist_reading, suppression(individual), 2024, 0.55).
narrative_ontology:measurement(wpn_inclusionist_grid_29, notability_guidelines__inclusionist_reading, suppression(organizational), 2006, 0.1).
narrative_ontology:measurement(wpn_inclusionist_grid_30, notability_guidelines__inclusionist_reading, suppression(organizational), 2024, 0.3).
narrative_ontology:measurement(wpn_inclusionist_grid_31, notability_guidelines__inclusionist_reading, suppression(structural), 2006, 0.4).
narrative_ontology:measurement(wpn_inclusionist_grid_32, notability_guidelines__inclusionist_reading, suppression(structural), 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, resource_allocation).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'WP:N' decomposes under the ε-invariance principle into one kernel with three readings; each reading instantiates a distinct constraint with its own ε over the same referent (the operated criteria arrangement on English Wikipedia). This file authors the inclusionist reading (high ε, snare claim). The deletionist reading supplies the legitimation narrative this reading critiques; the deliberative reading describes the process both others evaluate. Edges here express this reading's structural relations to its siblings; the family is complete only when all three files exist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
