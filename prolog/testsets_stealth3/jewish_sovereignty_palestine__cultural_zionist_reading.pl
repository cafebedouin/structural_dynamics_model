% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__cultural_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__cultural_zionist_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: jewish_sovereignty_palestine__cultural_zionist_reading
 *   human_readable: Cultural-Zionist Spiritual Center in Palestine (Non-Sovereign Reading)
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the cultural-Zionist reading of the contested
 *   kernel 'Jewish sovereignty in Palestine': the arrangement in which a
 *   Jewish cultural and spiritual center is built up in Palestine through
 *   voluntary immigration, diaspora funding, Hebrew-language
 *   institution-building, and land purchase, explicitly WITHOUT seeking
 *   political sovereignty or demographic majority, with the surrounding Arab
 *   population figured as co-inhabitants of a shared cultural space. The
 *   reading descends from Ahad Ha'am's line of thought, which treated the
 *   Jewish crisis as cultural rather than political and warned that
 *   sovereignty-seeking would import European power-politics into Jewish
 *   life. The arrangement operates from roughly 1890 to 1920, closing when
 *   the Balfour Declaration and the movement's political wing absorb the
 *   enterprise it seeded. KEY AGENTS (by structural relationship): -
 *   cultural_zionist_leadership: Agenda-setter (institutional/arbitrage) —
 *   designs and administers the center, defines the terms of coexistence -
 *   diaspora_jewish_communities: Funder-beneficiary (organized/constrained) —
 *   pays dues and donations, receives cultural vitality -
 *   hebrew_cultural_intelligentsia: Beneficiary (moderate/identity_locked) —
 *   vocations constituted by the center's existence -
 *   yishuv_pioneer_settlers: Beneficiary and receipt seat
 *   (moderate/constrained) — builds the center's material base, receives its
 *   flows - palestinian_arab_communities: Excluded cost-bearer
 *   (moderate/trapped) — co-inhabits without consent, bears incidental
 *   land-transfer costs - traditionalist_jewish_opponents: Excluded objector
 *   (organized/mobile) — rejects the arrangement from outside its
 *   institutions - nationalism_studies_scholars: Analytical observer
 *   (analytical/analytical) — sees all five readings' structures at once This
 *   file is one member of a five-story constraint family decomposing the
 *   colloquial label 'Zionism'. Its epsilon is authored low because the
 *   arrangement as designed requires no sovereignty machinery, no majority
 *   project, and no displacement regime; the sibling stories
 *   (statehood-as-right, divine-promise, settler-colonial, post-Zionist)
 *   carry materially different epsilon values and victim sets over
 *   overlapping physical facts.
 *
 * KEY AGENTS:
 *   - cultural_zionist_leadership: agenda-setter (institutional/arbitrage) — administers the center and authors its coexistence terms
 *   - diaspora_jewish_communities: funder-beneficiary (organized/constrained) — carries the funding burden, receives the cultural good
 *   - hebrew_cultural_intelligentsia: beneficiary (moderate/identity_locked) — professional existence fused to the center
 *   - yishuv_pioneer_settlers: beneficiary, receipt seat (moderate/constrained) — builds and receives the center's material base
 *   - palestinian_arab_communities: excluded cost-bearer (moderate/trapped) — resident population outside the design conversation
 *   - traditionalist_jewish_opponents: excluded objector (organized/mobile) — organized rejection from outside the arrangement
 *   - nationalism_studies_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__cultural_zionist_reading, 0.2).
domain_priors:suppression_score(jewish_sovereignty_palestine__cultural_zionist_reading, 0.15).
domain_priors:theater_ratio(jewish_sovereignty_palestine__cultural_zionist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__cultural_zionist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__cultural_zionist_reading, rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__cultural_zionist_reading, "Cultural-Zionist Spiritual Center in Palestine (Non-Sovereign Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__cultural_zionist_reading, "political philosophy / nationalism studies / postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__cultural_zionist_reading, '3bfa8e58-c3a4-4498-8881-b5b5af81abd2').
narrative_ontology:cs_kernel_codification('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', distributed).
narrative_ontology:cs_authority_grounding('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', distributed).
narrative_ontology:cs_reading_relation('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', foundational, cultural_renaissance_over_political_sovereignty).
narrative_ontology:cs_axiom_status(cultural_renaissance_over_political_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', cultural_renaissance_over_political_sovereignty, instrumental).
narrative_ontology:cs_axiom('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', foundational, palestine_as_shared_cultural_space).
narrative_ontology:cs_axiom_status(palestine_as_shared_cultural_space, holdable).
narrative_ontology:cs_axiom_grounding('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', palestine_as_shared_cultural_space, empirically_contingent).
narrative_ontology:cs_reference_frame('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', spiritual_center_without_sovereignty).
narrative_ontology:cs_drift_state('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', post_balfour_absorption_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3bfa8e58-c3a4-4498-8881-b5b5af81abd2', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__cultural_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_intelligentsia).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_pioneer_settlers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_pioneer_settlers).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the spiritual center from Odessa, Berlin, and London: founds Hebrew publishing houses and teacher seminaries, convenes congresses, solicits diaspora funds, purchases land through subsidiary bodies, and writes the terms under which settlers are to conduct themselves toward the surrounding population. Operates across European capitals and can redirect the project's aims, relocate its offices, or shift emphasis between cultural and political activity without leaving the movement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, cultural_zionist_leadership, agenda_setter,
    institutional, generational, arbitrage, continental).

% Sustain the center through shekel dues, box collections, and land-purchase donations; receive in return Hebrew literature, educational models, a living reference point for identity, and prestige accruing to contributing communities. Poorer communities feel the recurring solicitation most sharply, and declining to contribute carries social cost within communal standing. Stepping away means assimilating into host societies or joining rival movements such as the Bund or territorialism.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, diaspora_jewish_communities, payer).

% Writers, teachers, editors, and translators whose vocation exists only because the center exists: their language of work, their audiences, their institutional platforms, and their life projects are constituted by the Hebrew revival the center anchors. Leaving the arrangement would mean abandoning the medium of their life-work, not merely changing employers.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, hebrew_cultural_intelligentsia, beneficiary,
    moderate, biographical, identity_locked, continental).

% Build and staff the agricultural settlements, schools, and Hebrew-language infrastructure in Palestine; receive land, subsidies, and cultural purpose funded largely from abroad. Bear physical hardship, Ottoman immigration restrictions, wartime displacement, and the daily friction of building among a long-resident population whose consent was never formally sought. Funds, books, and institutional attention flow to this seat.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_pioneer_settlers, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_pioneer_settlers, payer).

% Inhabit the territory in which the center grows. Some landowners sell estates at market prices; some tenant farmers lose tenure when purchased estates change hands; villages supply wage labor to the new settlements; town merchants see new demand. The terms of coexistence are designed in Odessa, Berlin, and London without their participation; their objections travel as petitions to Ottoman provincial authorities, not as seats in the movement's congresses. Their lives are tied to land and village networks that cannot move.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_communities, excluded,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__cultural_zionist_reading, palestinian_arab_communities, payer).

% Rabbinic authorities who read the secular Hebrew revival as a rival to Torah, and diaspora autonomists who read emigration to Palestine as abandoning the struggle for rights where Jews actually live. They publish critiques, organize counter-institutions, and in some cases threaten communal sanctions, but hold no seat in the movement's decision-making bodies. They stand outside the arrangement by conviction and face no barrier remaining outside.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, traditionalist_jewish_opponents, excluded,
    organized, generational, mobile, continental).

% Analyze the arrangement comparatively: as a nationalism variant, as a colonization process, as a language-revival project, as a theology. See the full structure of the contested kernel and all of its competing readings at once, collect nothing from the arrangement, and bear none of its costs.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__cultural_zionist_reading, nationalism_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__cultural_zionist_reading, yishuv_pioneer_settlers).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__cultural_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of sustaining a dispersed people's language and culture under emancipation-era assimilation pressure: centralizes Hebrew literary production, teacher training, publishing, and a living demographic base for the revived language in one place, giving scattered communities a shared cultural reference point that no single diaspora community could maintain alone.
% TRANSFER_FUNCTION: Moves money (dues, donations, land-purchase funds) from diaspora communities to settlement and cultural institutions in Palestine; moves people (immigrants, teachers, students) and cultural goods (books, curricula) toward the center; moves cultural prestige and renewed identity materials back outward to the diaspora.
% ABSENT_VOICES: Palestinian Arab inhabitants are the principal absent voice: the arrangement's terms of coexistence were authored without their consent, and their objections surface only as petitions to Ottoman authorities. Traditionalist Jewish authorities and diaspora autonomists are also outside the room, objecting to the secular character of the revival and to emigration itself. None of these seats participated in designing the arrangement they live inside or argue against.
% DISAPPEARANCE_RATIONALE: If the center vanished overnight, the Hebrew revival loses its demographic and institutional anchor: the publishing houses, seminaries, and schools have no other base, the intelligentsia loses its audience and medium, diaspora communities lose the reference point around which contributions and identity materials circulate, and the settlements built on diaspora funds lose their supplying institutions. The whole cultural economy rearranges around whatever rival movements (autonomism, assimilation, territorialism) absorb the displaced energy.
% FOUNDING_PROBLEM: Post-emancipation European Jewry faced cultural dissolution: legal integration was eroding Hebrew literacy, communal affiliation, and collective identity faster than religious frameworks could sustain them. The reading's founders diagnosed the crisis as inner and cultural rather than political, arguing that sovereignty would not cure it and might import European power-politics into Jewish life; a spiritual center in Palestine would renew the people's inner life instead.
% FOUNDING_PROBLEM_CORROBORATION: Demographers and historians of emancipation-era Jewry, working outside the movement, document the assimilation-driven attrition of Hebrew literacy and communal affiliation that constitutes the founding problem. Contemporary Arab petitioners to Ottoman provincial authorities corroborate, from an adversarial seat, that the center physically existed and imposed unchosen costs on residents. No source outside the beneficiary set attests that the cultural-center remedy was the uniquely necessary response to the problem; that claim rests on the movement's own argument.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__cultural_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__cultural_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__cultural_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__cultural_zionist_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).
:- end_tests(jewish_sovereignty_palestine__cultural_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.20 at interval end) because the arrangement's core mechanisms are voluntary: immigration is chosen, contribution is solicited rather than levied by force, and the design nowhere requires displacing anyone. The residual 0.20 reflects real incidental costs the arrangement's operation generates — tenancy loss on transferred estates, cheap Arab wage labor in the settlements that the movement's own founders criticized, and the funding pressure on poor diaspora communities. Suppression is low (0.15): the arrangement holds by attraction and social pressure, not by coercive machinery; there is no enforcement apparatus preventing exit, and rivals (Bundism, territorialism, assimilation, religious quietism) operate openly. Theater ratio (0.30) tracks the growth of congresses, pilgrimages, and symbolic activity relative to the core productive function of publishing, teaching, and settlement — real function dominates, but ceremony accumulates as the movement institutionalizes. Accessibility collapse is low-moderate (0.35): understanding the cultural-center logic does not eliminate the alternatives; territorialism, autonomism, and assimilation remain visibly workable and actively argued. Resistance (0.45) is real and multi-front: Ottoman immigration restrictions, Arab petitions and protests over land sales, ultra-Orthodox counter-mobilization, and Bundist opposition. The claim (rope) and the metrics are authored independently: the claim states my structural judgment that this is genuine coordination solving a real collective-action problem at low coercive overhead; the metrics describe the arrangement's actual operation including its incidental costs. The measurement series run on one shared time grid (both metrics at all six points); the extractiveness series dips at the endpoint because the Great War devastated the yishuv's economy and the movement's cultural mandate was being absorbed into the political project, temporarily shrinking the arrangement's operational footprint.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same physical arrangement. From the leadership seat, the structure is a functioning voluntary enterprise it designed and maintains: coordination it built, overhead it watches, function it can point to. From the Palestinian seat, the same structure is an unchosen transformation of the landscape they inhabit — designed elsewhere, explained to them never, costing them land and tenancy they did not agree to surrender. From the diaspora seat the structure splits again: renewal gained, dues borne, with the burden falling unevenly by community wealth. The intelligentsia seat experiences the arrangement as the condition of professional existence itself. The engine computes these per-seat classifications from the structural data; the divergence between the leadership's computed type and the Palestinian seat's computed type is precisely the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: diaspora communities, the intelligentsia, and the settlers sit toward the beneficiary end; no structural victim class is declared because the arrangement's design requires no victims — its costs are incidental to operation rather than constitutive of it. Two overrides correct derivations the structural data alone cannot produce. Diaspora communities derive near-full-beneficiary from their beneficiary role, but they also carry the entire funding burden (secondary payer) and receive diffuse rather than concentrated goods; their net position sits nearer symmetric than the role alone suggests, hence d=0.30. Palestinian communities have no entry in the victims array, so the derivation lacks direct signal and would likely seat them near symmetric; their actual position is that of cost-bearing outsiders — trapped to the land, excluded from design, absorbing tenancy loss and labor-market subordination while receiving incidental commercial benefit — hence d=0.60. Neither override substitutes for structural declaration; both annotate positions the beneficiary/victim data underdetermines.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabelings guard this classification. First, romanticizing the reading as a timeless pure coordination: the temporal record shows theater accumulating and extractiveness creeping upward as land transfers scaled, and the arrangement's mandate was ultimately absorbed by the political wing rather than decaying in place — the story's interval therefore closes at absorption, not atrophy, and no piton signature is claimed. Second, dismissing the reading as mere cover for inevitable statehood: that question is routed to the coinhabitation_without_consent omega rather than settled by fiat, keeping the low-epsilon classification honest while flagging that it may be transient. The founding problem (assimilation-driven cultural dissolution) remains live, so the R5 mismatch consumer reads live-status paired with world_rearranges as consistent — no dead-mandate zombie flag fires. Mandatrophy resolution here prevents both the false snare (reading incidental land-transfer costs as the arrangement's purpose) and the false rope-certainty (ignoring that the arrangement's persistence depended on political conditions it did not control).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel jewish_sovereignty_palestine (the cultural_zionist_reading). What would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Track which reading''s institutional form prevails in the territory''s governance: the disagreement is located in the required institutional form of Jewish national fulfillment — spiritual center versus sovereign state versus theological commonwealth versus colonial-displacement regime versus post-national civic framework — and each resolution produces a different victim set, enforcement profile, and epsilon over the same geography.',
    'If the liberal-nationalist or religious reading prevails, effective extraction rises sharply as sovereignty machinery and majority projects activate; if the settler-colonial reading is adopted analytically, the same physical facts re-describe as extraction regardless of this reading''s intent; if the cultural reading had prevailed, the low-epsilon profile would be the stable terminal state rather than a transient phase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one-of-five readings of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    coinhabitation_without_consent,
    'Can a cultural center persist long-term among a much larger resident population that never consented to it, without drifting structurally toward sovereignty-seeking?',
    'Longitudinal analysis of the movement''s trajectory across political shocks: Ottoman collapse, British Mandate offers, and majority-demography projections. If every shock pushes the movement''s institutions toward statehood-seeking, the drift is structural rather than contingent.',
    'If drift toward sovereignty is structural, this reading''s low epsilon is transient and its classification migrates toward its siblings'' extractive profiles; if drift was contingent on specific historical accidents, the reading stands as a genuinely distinct low-extraction arrangement that history failed to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coinhabitation_without_consent, empirical, 'Whether non-sovereign co-inhabitation is stable or a waystation to statehood.').

omega_variable(
    land_purchase_displacement_scope,
    'How much of the arrangement''s actual operation involves tenancy loss through land transfer, and is that displacement incidental to or constitutive of the cultural-center mechanism?',
    'Land registry reconstruction: purchased tract areas, recorded tenant evictions, and compensation records versus total settlement footprint and institutional land holdings, decade by decade across the interval.',
    'If continuous land transfer from the resident population is constitutive — settlement cannot grow without it — the authored epsilon understates extraction and the reading converges structurally toward the settler-colonial sibling despite its non-sovereign intent; if displacement is bounded and incidental, the rope classification holds with the residual cost priced into epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_purchase_displacement_scope, empirical, 'Whether the arrangement''s land-transfer externalities are incidental costs or a structural dependency.').

omega_variable(
    diaspora_dues_voluntarity,
    'Is diaspora contribution (shekel dues, box collections, donations) genuinely voluntary, or socially compelled — and does the compulsion fall hardest on the poorest communities?',
    'Comparative analysis of community fundraising ledgers: exemption patterns, collection-pressure episodes, sanction cases against non-contributing members, and contribution rates correlated with community income.',
    'If contribution is effectively compulsory for communal standing, suppression is higher than authored, a payer seat exists inside Jewry itself, and the beneficiary framing of the diaspora seat partially collapses; if contribution is genuinely discretionary, the low-suppression authoring stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_dues_voluntarity, empirical, 'Voluntarity of the diaspora funding burden and its distribution across community wealth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__cultural_zionist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t6, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(jewi_tr_t12, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(jewi_tr_t18, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(jewi_tr_t24, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__cultural_zionist_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 0, 0.13).
narrative_ontology:measurement(jewi_be_t6, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 6, 0.15).
narrative_ontology:measurement(jewi_be_t12, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 12, 0.18).
narrative_ontology:measurement(jewi_be_t18, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 18, 0.21).
narrative_ontology:measurement(jewi_be_t24, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 24, 0.23).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__cultural_zionist_reading, base_extractiveness, 30, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_sovereignty_palestine__cultural_zionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__cultural_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__cultural_zionist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Zionism' / 'Jewish sovereignty in Palestine' decomposes into five structurally distinct arrangements linked through affects_constraints. Epsilon differs sharply across the family: this cultural reading requires no sovereignty machinery and no displacement regime by design (epsilon approximately 0.20); the liberal-nationalist and religious siblings instantiate state-enforced arrangements with majority projects and correspondingly higher extraction; the settler-colonial sibling re-describes overlapping physical facts as a displacement regime regardless of participant intent; the post-Zionist sibling evaluates the achieved state rather than the founding arrangement. The cultural reading is upstream of the liberal-nationalist sibling: the demographic and linguistic base it built is what made the political project feasible, which is why the influence edge runs in that direction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__cultural_zionist_reading, organized, 0.3).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__cultural_zionist_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
