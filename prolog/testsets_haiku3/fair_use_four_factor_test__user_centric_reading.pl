% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use as Affirmative User Right (Four-Factor Test)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The fair-use four-factor test (17 U.S.C. § 107) permits copying of
 *   copyrighted works without permission for purposes including criticism,
 *   commentary, news reporting, teaching, scholarship, and research, assessed
 *   via four factors: purpose and character of use, nature of the copyrighted
 *   work, amount used, and effect on the market value of the original. This
 *   reading instantiates fair use AS an affirmative user right — a structural
 *   protection of public access and cultural participation — rather than as a
 *   narrow exception to property right (creator-centric reading) or
 *   transformativeness-dominant doctrine (transformative-use reading). Under
 *   this reading, the four-factor test privileges preserving public access,
 *   educational use, and cultural production over rights-holder revenue
 *   maximization. Primary beneficiaries are educational institutions,
 *   researchers, commentators, and remix artists; primary victims are
 *   copyright holders and collecting entities. The extracted benefit is
 *   use-right transfer without licensing payment; the constraint persists
 *   through statutory authorization, judicial interpretation, and organized
 *   advocacy by beneficiary constituencies.
 *
 * KEY AGENTS:
 *   - Judicial system: agenda-setter; interprets and applies the four-factor test through case law; shapes actual boundaries of fair use
 *   - Educational institutions: primary beneficiary; use copyrighted material in pedagogy; benefit from free/low-cost content access
 *   - Copyright holders: payer; lose licensing revenue and exclusive-use rights under this reading; pursue narrowing interpretations
 *   - Cultural commentators and remix artists: beneficiaries; claim transformative use and public-benefit protection; constrained or identity-locked in exit
 *   - Public researchers: beneficiary; access scholarly and cultural materials for non-market research; large-scale research requires fee exemption
 *   - Rights-collecting entities: payer; administer licensing; trapped by statutory change; lose collection revenue under expansive fair-use reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.38).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.62).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use as Affirmative User Right (Four-Factor Test)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '8bd68e41-fc4a-4bcf-9c0e-1fc826934247').
narrative_ontology:cs_kernel_codification('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', fixed_text).
narrative_ontology:cs_authority_grounding('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', lineage).
narrative_ontology:cs_interpretation_layer_present('8bd68e41-fc4a-4bcf-9c0e-1fc826934247').
narrative_ontology:cs_reading_relation('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', fair_use_four_factor_test__creator_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', foundational, fair_use_as_affirmative_user_right).
narrative_ontology:cs_axiom_status(fair_use_as_affirmative_user_right, holdable).
narrative_ontology:cs_axiom_grounding('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', fair_use_as_affirmative_user_right, deontological).
narrative_ontology:cs_axiom('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', foundational, public_access_and_cultural_production_prioritized).
narrative_ontology:cs_axiom_status(public_access_and_cultural_production_prioritized, holdable).
narrative_ontology:cs_axiom_grounding('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', public_access_and_cultural_production_prioritized, instrumental).
narrative_ontology:cs_reference_frame('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', public_access_protected_right).
narrative_ontology:cs_drift_state('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', contemporary_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8bd68e41-fc4a-4bcf-9c0e-1fc826934247', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_commentators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_researchers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, remix_artists).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, fair_use_claimants).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, copyright_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, rights_collecting_entities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, cultural_commons_advocates).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, remix_artists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use copyrighted materials in classroom settings, course reserves, institutional repositories, and open educational resources without licensing fees. Benefit directly from free/low-cost access to textbooks, journals, multimedia, and research materials. Can negotiate licensing if fair use narrows, but aggregate cost would rise substantially and constrain content access. Organize collectively to defend fair-use doctrine through advocacy and litigation funding.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    institutional, generational, mobile, national).

% Quote, excerpt, and analyze copyrighted works in critical writing, journalism, video essays, and cultural commentary without licensing. Benefit from use-right that enables market entry and audience reach. Exit via licensing is economically unavailable (per-quote fees would make commentary unviable); exit via pure-original-analysis forgoes the rhetorical power of direct quotation and comparison.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_commentators, beneficiary,
    moderate, biographical, constrained, global).

% Access, digitize, and analyze copyrighted scholarship, datasets, historical materials, and cultural artifacts for scientific and historical research. Benefit from use-right that enables large-scale analysis (corpus linguistics, cultural analysis, data mining) infeasible under per-use licensing. Exit via institutional subscriptions covers only current commercial journals; archival, orphaned, and out-of-print materials require fair-use access or research stalls.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_researchers, beneficiary,
    moderate, biographical, constrained, global).

% Incorporate copyrighted music, visuals, film, and text into new creative works: mashups, remixes, fan art, video essays, collages. Identity is fused with remix practice; artistic expression is constituted through sampling and recontextualization. Benefit from fair-use doctrine that protects transformative use. Exit via wholly original creation forecloses their artistic identity; licensing is economically unavailable to emerging/non-commercial artists. They also bear suppression cost: risk of litigation and takedown notices creates chilling effect on ambitious remixing.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, remix_artists, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, remix_artists, payer).

% Hold exclusive reproduction and distribution rights; lose licensing revenue and control when uses are declared fair. Bear extraction via use authorization granted to beneficiaries without permission or payment. Maintain licensing infrastructure (mechanical licensing, performance rights, reprint permissions) and collect revenue from users who do license. Can arbitrage across jurisdictions with narrower fair-use doctrine; can mount litigation challenging fair-use claims and legislative lobbying for statutory narrowing.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, copyright_holders, payer,
    institutional, generational, arbitrage, global).

% Administer licensing and royalty collection on behalf of rights holders: ASCAP, BMI, SESAC for music; Copyright Clearance Center for reprints; Harry Fox Agency for mechanical licensing. Organizational mandate and revenue depend entirely on licensing activity. When fair use expands, licensing opportunities shrink and collection revenue declines. Cannot exit their administrative role without legislative change; statutorily trapped to licensing as their function.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, rights_collecting_entities, payer,
    institutional, generational, trapped, global).

% Individual, small-organization, and non-commercial users who are sued for copyright infringement and invoke fair-use defense. Have no seat at doctrine-setting; fair use is authored through litigation brought by institutional rights holders against individual defendants, creating a structural inverse: those defending fair use are those least able to afford litigation. Trapped by structural exclusion from the rulemaking process.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, fair_use_litigants, excluded,
    powerless, biographical, trapped, national).

% Organizations, networks, and constituencies (EFF, Creative Commons, academic freedom groups, archives) advocate for preservation and expansion of fair use, public domain, and open access. Benefit indirectly from fair-use expansion through expanded public access and reduced licensing pressures. Organize litigation support, legislative advocacy, and public education. Can move across jurisdictions and funding sources; mobile enough to sustain advocacy infrastructure.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, cultural_commons_advocates, beneficiary,
    organized, generational, mobile, global).

% Interprets and applies the four-factor test through case law. Courts weigh factors, decide individual cases, and progressively clarify doctrine through precedent. Judicial interpretation is the primary mechanism by which the constraint's boundaries shift. Courts are not beneficiary or payer but the mechanism by which the beneficiary/payer balance is institutionalized. Judges sit in contested position: institutional rights holders litigate regularly and invest in case selection; individual users rarely do, creating asymmetry in the cases judges see.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, judicial_system, agenda_setter,
    institutional, generational, analytical, national).

% Enact and amend copyright statutes. Can narrow or expand fair use through statutory amendment. Primary legislative pressure comes from rights-holder lobbying; fair-use beneficiary constituencies have less organized legislative capacity. Legislative action is episodic (1976 Act, 1992/1998 amendments, ongoing narrowing proposals). Most doctrine-setting occurs judicially between legislative moments.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, legislative_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, legislative_bodies, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates between copyright incentives (protecting creator investment) and public access (enabling cultural participation, scholarship, and derivative creation) by establishing a legal safety zone for certain uses that serve the public interest without requiring permission or payment from rights holders.
% TRANSFER_FUNCTION: Moves the right to use copyrighted material without licensing payment from rights holders to specific classes of users (educators, researchers, commentators, transformative creators) in categories defined by the four-factor test. The use flows to the designated beneficiaries; the loss flows to rights holders and collecting entities.
% ABSENT_VOICES: Individual copyright infringers sued under fair-use circumstances have no formal seat in doctrine-setting; they appear only as defendants in litigation controlled by institutional rights holders' strategy. Jurisdictions with expanding fair-use doctrine (EU, some non-common-law systems) are excluded from setting doctrine in US copyright law despite being affected by cross-border cultural flows.
% DISAPPEARANCE_RATIONALE: If the fair-use affirmative defense and its four-factor test disappeared overnight, educational institutions would require licensing for all classroom use, commentary and scholarship would require per-quote permission, cultural remixing would cease at large scale (becoming purely underground or commercially captured), and the public domain would stop enlarging through transformative reuse. The literary, academic, and cultural commons would reorganize around permission and payment.
% FOUNDING_PROBLEM: Copyright law's exclusive rights create a conflict between creator incentive (necessary to fund creation) and public access (necessary for cultural participation and derivative creation). Some copying must be free for education, research, and cultural commentary to function; the statute needed a safety valve to prevent copyright from foreclosing public benefit.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and courts defending fair use as central to First Amendment and copyright policy attest the founding problem is live. Copyright industries and rights holders attest the problem has been 'solved' by licensing infrastructure and that fair use has expanded beyond its intended scope into revenue-capture territory. Comparative law from EU (where fair use is narrower and licensing more comprehensive) shows the problem can be addressed differently; no consensus exists outside the US common-law tradition.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38) because the constraint transfers use-rights that would otherwise require licensing; the transfer is substantial but bounded by statute and case-law limits, and many beneficiaries (institutional users, commentators) have partial licensing alternatives. Suppression is higher (0.62) because the constraint's persistence requires active defense against judicial narrowing, legislative amendment, and rights-holder litigation; courts must actively weigh factors in beneficiary favor, and beneficiary organizations must mount litigation and advocacy to maintain boundaries. Theater is moderate (0.28): the four-factor test is genuinely applied in judicial settings, but the doctrine's rhetorical framing ('balancing') often obscures asymmetric power (institutional rights holders litigate regularly; individual users rarely do) and the outcome-steering effect of which cases are brought to court. Measurement series show base extractiveness rising from 0.25 to 0.41 across 1990–2010 (expansion of transformative-use doctrine, Google Books, Campbell v. Acuff-Rose, Cariou v. Prince), then stabilizing and slightly declining after 2010 as courts began implementing more restrictive four-factor application in response to rights-holder litigation strategy. Suppression requirement remains stable and elevated because courts must continuously defend the doctrine against narrowing pressure; the constraint is not naturally stable but must be actively maintained.
 *
 * PERSPECTIVAL GAP:
 *   From the copyright-holder seat (institutional, arbitrage-capable, powerful), the constraint appears as an uncompensated use authorization that erodes their exclusive rights and licensing revenue — extraction. From the educational institution or researcher seat (organized, constrained-exit, moderate-power), the constraint appears as essential coordination enabling public benefit — protection. From the remix artist or individual fair-use litigant seat (powerless, identity-locked, unorganized), the constraint appears as protection in theory but fragile in practice, since litigation is expensive and high-risk. From the judicial seat, the constraint appears as statutory interpretation requiring balancing — the judicial framing obscures that the balance has been shifting (user-centric readings gaining, then rights-holder litigation pushing back). The engine should compute different type classifications from each seat: institutional rights holders compute as snare (pure extraction via litigation threat and political pressure to narrow doctrine), educators compute as rope (genuine coordination), remix artists compute as tangled-rope (coordination benefit with identity-lock suppression), and judges compute as agenda-setter position neither benefiting nor paying but continuously contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Educational institutions and public researchers sit at low d (beneficiaries; constrained exit but coordinated alternatives available; benefit substantially from the constraint; organized enough to defend doctrine). Remix artists sit near low d as beneficiaries but with identity-locked exit, raising effective d toward mid-range. Copyright holders sit at high d (targets; arbitrage-capable but highly motivated to narrow the constraint; suffer extraction of licensing revenue; institutional power means they mount continuous litigation to narrow doctrine). Collecting entities sit at highest d (trapped target; entirely dependent on licensing revenue; cannot exit their role). Judicial system sits at mid-d as agenda-setter: it interprets the statute and applies the test, so its position is neither beneficiary nor payer but conditional operator — d derives from whether courts interpret narrowly (favoring rights holders, lower d) or expansively (favoring users, higher d). The user-centric reading authorship assumes courts interpret expansively; under the creator-centric reading, the same judges would sit lower. Directionality here derives from the reading's core premise: public access is the priority value; courts are expected (modeled) to weight factors in that direction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem (enable public access without eliminating creator incentive) remains live and contested, but the mechanism for solving it has shifted. Originally, fair use was a judicial safety valve — judges could declare uses fair case-by-case. As copyright expanded (term extension, digital transmission, statutory damages) and rights-holder litigation intensified, fair use narrowed in practical application even as doctrine nominally remained expansive. The constraint now functions partially as theater: the four-factor test is invoked and applied, but the outcomes are increasingly predictable (institutional users win narrow cases; individual users lose or settle). The constraint has not resolved its founding problem (the access-incentive tradeoff is no more settled now than in 1976); instead, it has generated a secondary mandate: courts must continuously re-defend the doctrine against legislative narrowing and rights-holder litigation. The mandatrophy risk is high: if the founding problem is deemed 'solved' by licensing infrastructure or market provision (music streaming, ebook platforms), the fair-use doctrine could be abandoned as obsolete, leaving only a narrow statutory exception. The user-centric reading is vulnerable to this mandatrophy scenario because it claims the broadest use-preservation; the creator-centric reading is more defensible on narrow-exception grounds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_doctrine_shifting,
    'Is the judicial application of the four-factor test genuinely user-protective, or has it shifted de facto toward rights-holder favorability through litigation gatekeeping and case-selection effects?',
    'Meta-analysis of published fair-use appellate decisions 1976–present, controlling for case type and litigant resources. Comparison with trial-court decisions and settlement patterns would show whether appellate doctrine is representative or selected.',
    'If de facto shifting has occurred, the constraint''s effective extractiveness is higher than the authored metrics suggest, and the constraint functions as tangled-rope (coordination framing with active suppression of unpopular uses) rather than rope. Type classification would shift from rope to tangled-rope at that seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_doctrine_shifting, empirical, 'Whether fair-use doctrine''s application has shifted despite stable statutory language.').

omega_variable(
    market_harm_factor_weight,
    'How should factor 4 (effect on market value of the original) be weighted relative to factors 1–3 (purpose, nature, amount) in the four-factor analysis? Should market harm be dispositive, one factor among four, or subordinated to transformativeness?',
    'Legislative amendment clarifying factor weights, or consistent appellate doctrine establishing a hierarchy. Comparative study of jurisdictions with codified factor weights (EU, other common-law systems) would show whether hierarchy is necessary or whether case-by-case balancing is tenable.',
    'If market harm is made dispositive (creator-centric reading wins), extractiveness drops (fewer uses qualify as fair); if transformativeness is made dominant (transformative-use reading wins), extractiveness stays moderate; if public-benefit factor is elevated above market harm (user-centric reading wins), extractiveness stays or rises. The type classification and the constraint''s survival depends entirely on which reading becomes canonical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_factor_weight, conceptual, 'How the four factors should be hierarchically weighted — the core contention between readings.').

omega_variable(
    licensing_availability_and_fair_use_scope,
    'To what extent should the availability and cost of licensing determine fair-use boundaries? Should fair use narrow as licensing infrastructure expands (music streaming, ebook platforms), or is fair use structurally independent of licensing availability?',
    'Appellate doctrine clarification. Campbell v. Acuff-Rose (transformative music sampling) addressed whether availability of licensing affected market-harm factor; subsequent cases (Google Books, Cariou v. Prince) refined this. The question is whether the trend continues toward licensing-availability narrowing fair use.',
    'If licensing availability becomes dispositive, fair use shrinks as licensing platforms mature, and extractiveness rises. If fair use is held independent of licensing, extractiveness remains stable. This is the mechanism by which mandatrophy (abandonment of the constraint as ''no longer needed'') could be executed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_availability_and_fair_use_scope, empirical, 'Whether licensing infrastructure maturity should narrow fair-use scope.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.62) primarily structural (litigation costs, rights-holder gatekeeping, judicial narrowing) or internalized (users internalize risk and self-censor, avoiding fair-use claims even when colorable)?',
    'Post-litigation behavior study: do users who win fair-use cases expand their use, or do they continue self-censoring? Survey of non-litigants about fair-use knowledge and perceived risk. EU comparison: jurisdictions with narrower fair use show similar self-censorship patterns despite lower litigation risk.',
    'If suppression is structural, removal of litigation threat would enable more uses. If internalized, even litigation removal would not expand use (users remain identity-locked to risk perception). This affects whether remedies (statutory clarification, fee-shifting) would succeed in expanding the constraint''s protective scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression operates through external legal/financial barriers or through internalized user risk-perception.').

omega_variable(
    user_centric_vs_creator_centric_kernel_framing,
    'Is the ''fair use as user right'' framing the statutorily intended reading, or is it a judicially-constructed expansion of a narrower legislative grant?',
    'Legislative history analysis (committee reports, floor debate on 1976 Act and 1992/1998 amendments). Comparison with legislative language in other jurisdictions (EU Directives). Analysis of whether doctrinal expansion has occurred despite stable statutory language.',
    'If user-centric framing is the original legislative intent, the constraint has simply been re-normalized after rights-holder encroachment (creator-centric reading is the deviation). If creator-centric was the original intent, the user-centric reading is a doctrinal expansion vulnerable to legislative reversion. The constraint''s stability and legitimacy depends on this historical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_centric_vs_creator_centric_kernel_framing, empirical, 'Whether the user-centric reading represents original legislative intent or later judicial expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__user_centric_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__user_centric_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fair_tr_t30, fair_use_four_factor_test__user_centric_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__user_centric_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(fair_tr_t50, fair_use_four_factor_test__user_centric_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(fair_be_t30, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(fair_be_t50, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fair_su_t10, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(fair_su_t20, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(fair_su_t30, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(fair_su_t40, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(fair_su_t50, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_four_factor_test__user_centric_reading, 0.12).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, copyright_term_extension_constraint).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, digital_millennium_copyright_act_circumvention_ban).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, music_licensing_collective_rights_management).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the fair_use_four_factor_test kernel. It is linked to two sibling readings instantiating the same kernel: creator_centric_reading (fair use as narrow exception protecting creator incentives) and transformative_use_reading (transformativeness as dominant factor). The three readings share the same statutory text (17 USC 107) but instantiate structurally different constraints with different epsilon, beneficiary sets, victim sets, and type classifications. Each reading is a complete story; the trilogy together models how a single kernel generates multiple, coexisting constraint structures depending on which reading becomes doctrinal canon. The user-centric reading influences (but does not foreclose) the transformative-use reading — expansive transformativeness doctrine enables user-beneficial outcomes. The creator-centric reading forecloses the user-centric reading: if market harm is made dispositive, affirmative user-right framing is logically impossible. The three are not stages of doctrinal evolution but simultaneously-live positions whose relative institutional dominance shifts with litigation, legislation, and court appointments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_four_factor_test__user_centric_reading, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
