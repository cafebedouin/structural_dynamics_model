% ============================================================================
% CONSTRAINT STORY: gdpr_article_3_scope__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gdpr_article_3_scope__market_access_reading, []).

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
 *   constraint_id: gdpr_article_3_scope__market_access_reading
 *   human_readable: GDPR Article 3 Extraterritorial Reach as Conditional Market Access (Brussels Effect Reading)
 *   domain: technology_governance/international_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading, the market_access_reading, of the
 *   contested kernel gdpr_article_3_scope: what GDPR's Article 3(2)
 *   extraterritorial reach IS. On this reading the operative mechanism is
 *   conditional market access: any firm worldwide that wants EU revenue
 *   complies with the EU rulebook, and the resulting global standardization
 *   is the Brussels Effect, standard diffusion through market gravity, rather
 *   than a jurisdictional assertion projected by EU courts onto foreign
 *   territory. The standing arrangement under contest, and therefore the
 *   epsilon referent, is that arrangement as it actually operates: Article
 *   3(2)'s targeting and monitoring criteria binding foreign firms, the
 *   compliance cascade through multinational infrastructures, the adequacy
 *   network, and the clone-law wave across sixty-plus jurisdictions. Per the
 *   epsilon-invariance principle this file contains ONLY this reading: the
 *   effects_jurisdiction_reading (jurisdiction follows effects;
 *   compulsion-indexed) and the territorial_sovereignty_reading (illegitimate
 *   projection; legitimacy-violation-indexed) are separate constraint stories
 *   with their own epsilon values, linked through
 *   network.affects_constraints. Claim and metrics are independent authored
 *   facts: the claimed type (tangled_rope) is asserted from structure, a
 *   genuine harmonization function carrying asymmetric extraction, while the
 *   metrics are authored descriptively from the arrangement's observed
 *   operation; the engine computes per-seat classifications from the
 *   structural data, and any divergence from the claim is the datum the
 *   corpus exists to take. KEY AGENTS (by structural relationship): -
 *   eu_co_legislators: Agenda-setter (institutional/arbitrage) — authors the
 *   market-access condition and collects standard-setting influence as the
 *   rulebook diffuses - eu_data_protection_authorities: Enforcer seat
 *   (institutional/constrained) — makes the condition credible through
 *   supervision and sanction - eu_residents_data_subjects: Primary intended
 *   beneficiary (organized/constrained) — holds enforceable rights under the
 *   single rulebook - compliant_multinational_platforms: Dual
 *   payer-beneficiary (powerful/constrained) — pays compliance costs,
 *   collects moat and single-standard rents -
 *   surveillance_adtech_intermediaries: Primary target
 *   (powerful/identity_locked) — bears business-model foreclosure -
 *   small_independent_publishers: Diffuse payer (powerless/constrained) —
 *   bears consent overhead and yield collapse -
 *   non_eu_smes_serving_eu_market: Secondary payer (moderate/constrained) —
 *   bears regressive compliance fixed costs -
 *   non_eu_regulators_adopting_gdpr_clones: Converted beneficiary
 *   (institutional/constrained) — imports the rulebook, cedes agenda
 *   initiative - compliance_industry_vendors: Parasitic beneficiary
 *   (organized/arbitrage) — receives the pecuniary compliance flows -
 *   us_federal_regulatory_establishment: Excluded rival standard-setter
 *   (institutional/arbitrage) — its sectoral model was crowded out of the
 *   design conversation - privacy_civil_society_organizations: Mission-fused
 *   beneficiary (organized/identity_locked) — polices enforcement gaps within
 *   the architecture it depends on - comparative_regulation_analysts:
 *   Analytical observer (analytical/analytical) — measures the diffusion and
 *   enforcement that both camps cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, 0.52).
domain_priors:suppression_score(gdpr_article_3_scope__market_access_reading, 0.48).
domain_priors:theater_ratio(gdpr_article_3_scope__market_access_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gdpr_article_3_scope__market_access_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gdpr_article_3_scope__market_access_reading, tangled_rope).
narrative_ontology:human_readable(gdpr_article_3_scope__market_access_reading, "GDPR Article 3 Extraterritorial Reach as Conditional Market Access (Brussels Effect Reading)").
narrative_ontology:topic_domain(gdpr_article_3_scope__market_access_reading, "technology_governance/international_law").

domain_priors:requires_active_enforcement(gdpr_article_3_scope__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gdpr_article_3_scope__market_access_reading, '6f9077d0-09f0-44dd-989e-6b878d6b8b46').
narrative_ontology:cs_kernel_codification('6f9077d0-09f0-44dd-989e-6b878d6b8b46', formalized).
narrative_ontology:cs_authority_grounding('6f9077d0-09f0-44dd-989e-6b878d6b8b46', extraction).
narrative_ontology:cs_interpretation_layer_present('6f9077d0-09f0-44dd-989e-6b878d6b8b46').
narrative_ontology:cs_reading_relation('6f9077d0-09f0-44dd-989e-6b878d6b8b46', gdpr_article_3_scope__effects_jurisdiction_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f9077d0-09f0-44dd-989e-6b878d6b8b46', gdpr_article_3_scope__territorial_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('6f9077d0-09f0-44dd-989e-6b878d6b8b46', foundational, extraterritoriality_is_standard_diffusion_not_jurisdictional_assertion).
narrative_ontology:cs_axiom_status(extraterritoriality_is_standard_diffusion_not_jurisdictional_assertion, holdable).
narrative_ontology:cs_axiom_grounding('6f9077d0-09f0-44dd-989e-6b878d6b8b46', extraterritoriality_is_standard_diffusion_not_jurisdictional_assertion, empirically_contingent).
narrative_ontology:cs_axiom('6f9077d0-09f0-44dd-989e-6b878d6b8b46', foundational, compliance_is_rational_market_strategy).
narrative_ontology:cs_axiom_status(compliance_is_rational_market_strategy, holdable).
narrative_ontology:cs_axiom_grounding('6f9077d0-09f0-44dd-989e-6b878d6b8b46', compliance_is_rational_market_strategy, instrumental).
narrative_ontology:cs_reference_frame('6f9077d0-09f0-44dd-989e-6b878d6b8b46', conditional_market_access_rulebook).
narrative_ontology:cs_drift_state('6f9077d0-09f0-44dd-989e-6b878d6b8b46', contemporary_post_diffusion_wave, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('6f9077d0-09f0-44dd-989e-6b878d6b8b46', '').
narrative_ontology:cs_kernel_id(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_residents_data_subjects).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, eu_co_legislators).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, compliance_industry_vendors).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, privacy_civil_society_organizations).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, non_eu_regulators_adopting_gdpr_clones).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, surveillance_adtech_intermediaries).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, small_independent_publishers).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_smes_serving_eu_market).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gdpr_article_3_scope__market_access_reading, compliant_multinational_platforms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, compliant_multinational_platforms).
narrative_ontology:constraint_victim(gdpr_article_3_scope__market_access_reading, non_eu_regulators_adopting_gdpr_clones).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, brussels_effect_standard_diffusion).
narrative_ontology:constraint_vindicates(gdpr_article_3_scope__market_access_reading, compliance_as_competitive_strategy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parliament, Council, and Commission authored Regulation 2016/679 and set the conditions under which any firm worldwide may serve EU users. They harvest standard-setting influence as jurisdictions clone the rulebook, externalizing enforcement to firms' own compliance departments. They can amend or repeal the text at will, though constitutional entrenchment in several member states and the adequacy network raise the political price of doing so.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_co_legislators, agenda_setter,
    institutional, generational, arbitrage, continental).

% National supervisory authorities and the EDPB supervise compliance and issue corrective powers and fines. Their credible sanctioning capacity is what makes market access conditional rather than advisory. Caseloads chronically exceed budgets; large-platform investigations consume years while the long tail of smaller violators goes largely unpoliced.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_data_protection_authorities, agenda_setter,
    institutional, biographical, constrained, continental).

% Hold enforceable rights of access, erasure, portability, and objection under a single redress channel spanning the union. Individually weak against platforms, collectively represented through consumer organizations, digital-rights groups, and a growing class-action channel. Cannot exit digital services wholesale; consent fatigue and dark-pattern interfaces discount the value of the rights they formally hold.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, eu_residents_data_subjects, beneficiary,
    organized, biographical, constrained, continental).

% Operate global infrastructure under one compliance regime: data-protection officers, records of processing, breach pipelines, rights-fulfillment engineering. Pay heavy fixed compliance costs and recoup part of them through moat effects (the same fixed costs deter entrants), trust signaling, and single-standard economies that beat managing fifty fragmented regimes. Exiting the EU market is commercially unthinkable; case-by-case litigation is their main resistance channel.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, compliant_multinational_platforms, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, compliant_multinational_platforms, beneficiary).

% Behavioral-tracking middlemen whose inventory value depends on the cross-site identifiers and lawful-basis chains the regulation curtails. Consent requirements collapsed open-web bidding yields and forced contextual-advertising experiments. Their organizational competence, data assets, and customer relationships are all fused with behavioral monetization, so pivoting away dissolves the firm rather than relocating it; they litigate, lobby, and route around instead of exiting.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, surveillance_adtech_intermediaries, payer,
    powerful, biographical, identity_locked, global).

% Monetize content through programmatic advertising. Consent-management overhead, identifier loss, and consent-wall friction cut auction yields sharply while compliance tooling is priced as a fixed cost they cannot amortize. Individually too small to negotiate with platforms or litigate; trade-body membership and advertiser coalitions are their only collective channels. Leaving the open web means paywalls few readers purchase.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, small_independent_publishers, payer,
    powerless, biographical, constrained, global).

% Firms outside the EU selling software and services to EU customers must stand up compliance programs sized for EU law regardless of home jurisdiction. Costs are regressive relative to revenue and arrive without any voice in the rulemaking. Dropping EU customers is possible but usually sacrifices the highest-margin segment, so most absorb the cost and pass part of it to prices.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_smes_serving_eu_market, payer,
    moderate, biographical, constrained, national).

% Data-protection authorities and legislators from Brasilia to Tokyo import the GDPR template because drafting and maintaining a divergent regime invites market friction and forfeits adequacy pathways. They gain a tested rulebook, ready-made supervisory doctrine, and mutual-recognition access; they cede agenda-setting initiative to Brussels and inherit enforcement burdens their budgets strain to meet.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, non_eu_regulators_adopting_gdpr_clones, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(gdpr_article_3_scope__market_access_reading, non_eu_regulators_adopting_gdpr_clones, payer).

% Consent-management platforms, compliance suites, consultancies, auditors, and training providers. Revenue scales with the strictness and geographic spread of the requirement, and the same tooling sells into every new jurisdiction that clones the standard. They bear no exposure to the underlying data businesses and benefit from stricter rather than looser enforcement.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, compliance_industry_vendors, beneficiary,
    organized, biographical, arbitrage, global).

% Prefers a sectoral, light-touch privacy model and had no seat in GDPR's design. Objects diplomatically to extraterritorial reach and to the adequacy gatekeeping of US intelligence law. Responds by fostering state-level statutes in the CCPA lineage that nonetheless converge toward GDPR-shaped provisions, meaning its preferred model was crowded out of the standard-setting conversation even as its jurisprudence drifts toward the winning template.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, us_federal_regulatory_establishment, excluded,
    institutional, generational, arbitrage, continental).

% Digital-rights NGOs gained statutory consultation roles, litigation standing, complaint channels, and funding relevance from the regime. Their mission identity is fused with the rights architecture they police: they campaign against enforcement gaps and dark patterns, not against the architecture, because the architecture is what gives them standing.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, privacy_civil_society_organizations, beneficiary,
    organized, generational, identity_locked, continental).

% Scholars and think-tanks tracking diffusion: adequacy-decision counts, clone-law genealogies, enforcement statistics, compliance-cost estimates. Publish the measurements that defenders and critics of the arrangement alike cite, and hold no material stake in the compliance flows they measure.
narrative_ontology:constraint_stakeholder(gdpr_article_3_scope__market_access_reading, comparative_regulation_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gdpr_article_3_scope__market_access_reading, compliance_industry_vendors).
narrative_ontology:fixing_cost_class(gdpr_article_3_scope__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single enforceable data-protection rulebook for any firm serving the EU's roughly 450 million consumers: one compliance regime replaces twenty-seven divergent national ones, data-subject rights become portable across borders, and international data flows gain a common trust baseline.
% TRANSFER_FUNCTION: Moves compliance expenditure (money, engineering hours, organizational attention) from every firm seeking EU market access into compliance infrastructure and vendor fees; moves standard-setting initiative from dispersed national regulators to the EU rulebook; moves enforceable data-control rights to EU residents.
% ABSENT_VOICES: Non-EU data subjects, several billion people now governed by GDPR-derived rules, were never consulted and hold no vote in the institutions that set the standard. US federal regulators preferring a sectoral model stood outside the design conversation. Small publishers' unit economics entered only through late industry lobbying. They sit outside the EU legislative process, represented only indirectly through lobby submissions and adequacy diplomacy.
% DISAPPEARANCE_RATIONALE: Member-state regimes would re-fragment overnight, returning to the 1995 Directive patchwork; adequacy decisions and the mutual-adequacy network would lapse; the hundred-plus GDPR-derived laws would lose their anchor text and begin diverging; the compliance-tooling industry would contract sharply; and cross-border tracking practices would re-expand into the EU within quarters.
% FOUNDING_PROBLEM: Data protection under the 1995 Directive was transposed unevenly across member states, leaving rights unenforceable across borders in a digital single market, while platform-scale behavioral data collection outpaced national supervisory capacity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic measurement studies document continued large-scale tracking and consent-flow dark patterns; non-EU regulators attest the same underlying problem in their own jurisdictions during adequacy negotiations and OECD work; breach-notification statistics and DPA enforcement reports show violations continuing at scale. No source outside the beneficiary set attests that the founding problem is solved.
narrative_ontology:disappearance_verdict(gdpr_article_3_scope__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(gdpr_article_3_scope__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gdpr_article_3_scope__market_access_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gdpr_article_3_scope__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gdpr_article_3_scope__market_access_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gdpr_article_3_scope__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gdpr_article_3_scope__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gdpr_article_3_scope__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the arrangement's transfers beyond coordination cost are real but moderate on this reading's lights — vendor-fee flows, incumbent-moat consolidation, crowding-out of divergent regulatory designs, and compliance burdens on firms and publics that never voted for the rulebook, all riding on a harmonization function that also delivers genuine surplus (single-regime economics, portable rights). Suppression 0.48 is authored as a raw structural property, unscaled by power or scope: coercion here is market-structural rather than juridical — exit means abandoning the EU's roughly 450-million-consumer market or geofencing it, both commercially severe but neither legally forbidden — and divergent privacy designs face crowding-out rather than prohibition. Theater ratio 0.40: consent-banner ritualization, unread notices, and pay-or-consent dark patterns form a large performative stratum, but the core machinery (records of processing, breach notification, the DPO function, rights fulfillment) is functional. Accessibility collapse 0.62: once a firm understands the constraint, alternatives collapse sharply — no rival privacy regime excuses EU-facing noncompliance — but geofencing and market abstention remain real, if rarely taken, exits. Resistance 0.52: a decade of sustained industry lobbying, litigation in the Schrems lineage and around pay-or-consent models, and diplomatic objection from third countries, short of frontal revolt because compliance is individually rational. Identity-lock dynamics: surveillance_adtech_intermediaries are organizationally fused with behavioral-monetization competence, so exit dissolves the firm's core identity rather than relocating it, and privacy_civil_society_organizations are mission-fused with the rights architecture they audit; both seats compute as more trapped than their balance sheets suggest. Coalition potential: small_independent_publishers are individually powerless but hold class-channel leverage through trade bodies, advertiser coalitions, and emerging class actions that a purely individual-level read misses. Temporal series run on one shared grid (t=0 through t=10, all three metrics authored at every point); trajectories are monotone — no oscillation is authored because the drift over this interval is directional rather than cyclical: extraction accumulates as compliance industrializes while the suppression requirement plateaus as market self-enforcement substitutes for coercive enforcement, which is precisely this reading's lower-enforcement-tension signature.
 *
 * PERSPECTIVAL GAP:
 *   The same statutory text computes as different constraints from different seats. From the eu_co_legislators and eu_data_protection_authorities seats the arrangement is legitimate governance whose extraterritoriality is benign diffusion — influence collected without imperial administration. From the compliant_multinational_platforms seat it is a heavy but manageable tax that doubles as a moat: the same fixed costs that burden them deter their next competitor, which is why this seat pays and benefits simultaneously. From the surveillance_adtech_intermediaries seat it is existential foreclosure enforced by market structure rather than court order — no judge shut them down; their inventory simply stopped clearing. From the small_independent_publishers and non_eu_smes_serving_eu_market seats it is an unfunded mandate priced in regressive fixed costs. From the non_eu_regulators seat it is a gift that costs autonomy: a tested rulebook acquired at the price of agenda-setting initiative. The engine computes these divergent per-seat classifications from power, exit, and role data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation. eu_co_legislators (beneficiary, arbitrage exit) sit nearest the beneficiary pole: they collect standard-setting influence rents while externalizing enforcement to firms' own compliance departments. compliance_industry_vendors (pure beneficiary, arbitrage) sit nearly as low: revenue scales with strictness and spread, with no exposure to the underlying data businesses. eu_residents_data_subjects (beneficiary, constrained exit) sit low but not minimal — protection is real, discounted by consent fatigue and dark-pattern erosion. compliant_multinational_platforms carry role=payer with secondary_role=beneficiary, so the derivation sees the compliance payment and the moat and single-standard rents together, placing them mid-scale rather than at the target pole; the residual uncertainty about how much of their cost is moat versus rights delivery is carried by omega incumbent_moat_share_of_compliance_cost rather than by a directionality override, since a power-atom-keyed override could not separate them from surveillance_adtech_intermediaries, who share the powerful atom but sit near the full-target pole as payers with identity_locked exit — their organizational identity IS behavioral monetization, so exit is dissolution. small_independent_publishers (payer, powerless, constrained) and non_eu_smes_serving_eu_market (payer, moderate, constrained) sit high. non_eu_regulators_adopting_gdpr_clones carry beneficiary with secondary_role=payer — rulebook gained, agenda ceded — landing them modestly above the beneficiary pole. eu_data_protection_authorities and us_federal_regulatory_establishment declare no beneficiary or victim position (administrator and excluded respectively) and take power-atom fallbacks; this is deliberate — administrators run the constraint without collecting its rents, and the excluded seat's grievance is representational, not extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmentary, cross-border-unenforceable data protection against platform-scale collection — remains live, corroborated by tracking studies, enforcement statistics, and non-EU regulator testimony, so mandatrophy is not resolved and no sunset dynamics are authored. The tangled_rope classification is what prevents mislabeling in both directions: a rope reading would erase the asymmetric extraction (crowding-out of alternative regulatory designs, incumbent moats, unrepresented publics, vendor-fee flows); a snare reading would erase the genuine coordination surplus (single-regime economics that even paying firms prefer to fragmentation) and the strategic, individually rational character of compliance that distinguishes this arrangement from coerced extraction. The R5 mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges — no zombie flag fires; the arrangement persists because the problem persists, not because the problem left and the paperwork stayed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_of_kernel_disagreement,
    'This story fixes the market_access_reading of kernel gdpr_article_3_scope; the same Article 3(2) arrangement under the effects_jurisdiction_reading indexes epsilon to legal compulsion of foreign firms, and under the territorial_sovereignty_reading to legitimacy violation against third-country regulatory autonomy. Which mechanism characterization governs the classification?',
    'Observe the binding channel empirically: whether foreign-firm behavior changes in response to DPA court orders and cross-border enforcement instruments, or to market-access calculations, or both, and in what proportion, using enforcement-action timelines against compliance-adoption timelines.',
    'Re-indexing to the effects reading raises suppression and shifts the beneficiary seat toward EU judicial organs; re-indexing to the territorial reading adds third-country polities to the victim set and pushes the classification toward snare. The market-access indexing keeps extraction moderate and enforcement tension low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_index_of_kernel_disagreement, conceptual, 'Which sibling reading''s mechanism characterization governs classification of the shared Article 3(2) arrangement.').

omega_variable(
    voluntariness_depth_of_compliance,
    'Is compliance under the market-access mechanism genuinely strategic choice among real alternatives, or formally voluntary but practically compelled because exiting the EU market is not a live option for most firms?',
    'Count revealed choices: persistence of post-2018 geofencing, documented EU-exit deliberations, behavior of firms with marginal EU revenue share, and stated reasoning in annual filings and litigation.',
    'If compliance is compelled in practice, this reading''s low-enforcement-tension signature is overstated, suppression is understated, and the classification drifts snare-ward; if genuinely strategic, the rope component of the tangled rope is stronger than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_depth_of_compliance, empirical, 'Whether market-access compliance is deep voluntariness or compelled choice wearing voluntary form.').

omega_variable(
    incumbent_moat_share_of_compliance_cost,
    'What share of measured compliance cost functions as entry deterrence consolidating incumbent platforms, rather than as rights-delivery expenditure?',
    'Differential startup formation and survival rates in EU-facing digital sectors before and after May 2018; ad-tech and martech concentration trends; natural experiments from enforcement lulls and member-state variation.',
    'A high moat share raises effective extraction on the small-firm payer seats and supports drift toward snare; a low share supports the rope-leaning face of the tangled rope and validates the reading''s compliance-as-strategy axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_moat_share_of_compliance_cost, empirical, 'How much of the compliance burden is competitive moat versus genuine protection cost.').

omega_variable(
    procedural_legitimacy_of_unrepresented_data_subjects,
    'Non-EU data subjects, billions of people now governed by GDPR-derived rules they cannot vote on, sit in which column: beneficiaries of exported protection, or victims of extracted regulatory self-determination?',
    'Not resolvable by data alone: requires a normative weighting of substantive protection against procedural self-determination. Comparative welfare outcomes (breach rates, remedy access inside versus outside the EU) inform but cannot settle the weighting.',
    'Placing unrepresented publics in the victim column adds the largest affected population to the target side and materially raises aggregate extraction; placing them among beneficiaries keeps the arrangement''s beneficiary mass dominant. The swing is the single largest directionality uncertainty in the story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_legitimacy_of_unrepresented_data_subjects, preference, 'Whether exported protection without representation counts as subsidy or imposition.').

omega_variable(
    diffusion_durability_under_declining_market_gravity,
    'Does standard diffusion persist if the EU''s share of global digital-economic gravity declines relative to US and Chinese regulatory poles?',
    'Track clone-law retention and adequacy renewals as EU digital-revenue share falls; watch for rival-pole standards (US federal privacy legislation, China''s PIPL sphere) attracting divergent compliance stacks.',
    'Decay would strip the mechanism from the cloned shells and convert surviving copies toward piton, theatrical compliance maintained by inertia without the market condition that funds it; durable diffusion confirms the market-access mechanism as load-bearing rather than decorative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffusion_durability_under_declining_market_gravity, empirical, 'Whether the Brussels Effect mechanism survives erosion of the EU market gravity that powers it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gdpr_article_3_scope__market_access_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gdpr_tr_t0, gdpr_article_3_scope__market_access_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(gdpr_tr_t0, observed).
narrative_ontology:measurement(gdpr_tr_t2, gdpr_article_3_scope__market_access_reading, theater_ratio, 2, 0.26).
narrative_ontology:measurement_basis(gdpr_tr_t2, observed).
narrative_ontology:measurement(gdpr_tr_t4, gdpr_article_3_scope__market_access_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement_basis(gdpr_tr_t4, observed).
narrative_ontology:measurement(gdpr_tr_t6, gdpr_article_3_scope__market_access_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(gdpr_tr_t6, observed).
narrative_ontology:measurement(gdpr_tr_t8, gdpr_article_3_scope__market_access_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement_basis(gdpr_tr_t8, observed).
narrative_ontology:measurement(gdpr_tr_t10, gdpr_article_3_scope__market_access_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement_basis(gdpr_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(gdpr_be_t0, gdpr_article_3_scope__market_access_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(gdpr_be_t0, observed).
narrative_ontology:measurement(gdpr_be_t2, gdpr_article_3_scope__market_access_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement_basis(gdpr_be_t2, observed).
narrative_ontology:measurement(gdpr_be_t4, gdpr_article_3_scope__market_access_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement_basis(gdpr_be_t4, observed).
narrative_ontology:measurement(gdpr_be_t6, gdpr_article_3_scope__market_access_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement_basis(gdpr_be_t6, observed).
narrative_ontology:measurement(gdpr_be_t8, gdpr_article_3_scope__market_access_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(gdpr_be_t8, observed).
narrative_ontology:measurement(gdpr_be_t10, gdpr_article_3_scope__market_access_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(gdpr_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(gdpr_su_t0, gdpr_article_3_scope__market_access_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(gdpr_su_t0, observed).
narrative_ontology:measurement(gdpr_su_t2, gdpr_article_3_scope__market_access_reading, suppression_requirement, 2, 0.34).
narrative_ontology:measurement_basis(gdpr_su_t2, observed).
narrative_ontology:measurement(gdpr_su_t4, gdpr_article_3_scope__market_access_reading, suppression_requirement, 4, 0.39).
narrative_ontology:measurement_basis(gdpr_su_t4, observed).
narrative_ontology:measurement(gdpr_su_t6, gdpr_article_3_scope__market_access_reading, suppression_requirement, 6, 0.43).
narrative_ontology:measurement_basis(gdpr_su_t6, observed).
narrative_ontology:measurement(gdpr_su_t8, gdpr_article_3_scope__market_access_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(gdpr_su_t8, observed).
narrative_ontology:measurement(gdpr_su_t10, gdpr_article_3_scope__market_access_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(gdpr_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gdpr_article_3_scope__market_access_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__effects_jurisdiction_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, gdpr_article_3_scope__territorial_sovereignty_reading).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, eu_ai_act_extraterritorial_diffusion).
narrative_ontology:affects_constraint(gdpr_article_3_scope__market_access_reading, california_ccpa_market_convergence).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'GDPR extraterritoriality' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints indexed by mechanism characterization of the same statutory text. This story (market_access_reading) carries the moderate, strategy-indexed epsilon: extraction as market-structural transfer with low enforcement tension. gdpr_article_3_scope__effects_jurisdiction_reading carries the compulsion-indexed epsilon (higher suppression; beneficiary seat shifts toward EU judicial and enforcement organs). gdpr_article_3_scope__territorial_sovereignty_reading carries the legitimacy-indexed epsilon (third-country regulatory self-determination enters the victim set; snare-leaning). The effects reading is upstream in enforcement practice, since its characterization is what DPAs plead in court; this market-access reading is the empirical-mechanism layer that explains why observed enforcement tension stays low despite formal reach; the territorial reading is the standing normative objection layer. Edges to eu_ai_act_extraterritorial_diffusion and california_ccpa_market_convergence record downstream diffusion: this constraint demonstrated and normalized the conditional-market-access playbook that the AI Act extends and that California's law converged toward.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
