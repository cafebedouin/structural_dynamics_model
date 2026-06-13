% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi Article II: Māori Tino Rangatiratanga (Rangatiratanga Reading)
 *   domain: constitutional/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) was signed in two versions: the English
 *   text (Article I) by which Māori ceded sovereignty to the Crown, and the
 *   Māori-language text (Article II) by which Māori guaranteed the Crown's
 *   kāwanatanga (authority over non-Māori affairs) while retaining their own
 *   tino rangatiratanga (full authority) over lands, resources, and taonga.
 *   These texts are not translations of each other — they establish different
 *   distributions of power. The rangatiratanga reading holds that Article
 *   II's language is binding and carries the true intent of Māori
 *   signatories; it constructs the Crown's authority as limited to settlers
 *   and non-Māori affairs, leaving Māori with retained inherent sovereignty.
 *   This reading is one of three contested instantiations of the
 *   waitangi_sovereignty_allocation kernel; it coexists with the
 *   crown_sovereignty_reading (English text precedence, complete cession) and
 *   the partnership_reading (ongoing consultative relationship without
 *   structural authority transfer). The metrics describe this constraint's
 *   actual operation: extraction has declined over 184 years as the
 *   rangatiratanga reading gained political and institutional weight
 *   (settlements, tribunal findings, legislative amendments), but suppression
 *   remains high because the Crown continues to defend statutory supremacy
 *   and resist full authority transfer. Theater has risen sharply as the
 *   constraint's enforcement shifted from outright land theft to performative
 *   consultation and co-governance consultation (consultation without binding
 *   power, committees without decision authority).
 *
 * KEY AGENTS:
 *   - Māori iwi collective: Organized authority claiming tino rangatiratanga over ancestral territories and resources; beneficiary and agenda-setter under this reading.
 *   - Crown statutory authority: Institutional actor defending parliamentary supremacy and Crown control of resource management; payer and constrained agenda-setter.
 *   - Settler land claimants: Moderate power actors whose title security depends on Crown's ability to alienate Māori lands; payers whose exit is geographically and legally constrained.
 *   - Treaty settlement beneficiaries: Iwi who have received Crown redress and co-governance seats; organized beneficiaries whose status is identity-constitutive.
 *   - Waitangi Tribunal: Institutional observer that recommends remedies aligned with the rangatiratanga reading; advises but cannot enforce.
 *   - Crown judiciary: Institutional observer that interprets Treaty principles; gradually shifting toward acknowledging rangatiratanga.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.31).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.67).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi Article II: Māori Tino Rangatiratanga (Rangatiratanga Reading)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '8c34ccd2-d33a-4dbe-940f-451537310766').
narrative_ontology:cs_kernel_codification('8c34ccd2-d33a-4dbe-940f-451537310766', fixed_text).
narrative_ontology:cs_authority_grounding('8c34ccd2-d33a-4dbe-940f-451537310766', extraction).
narrative_ontology:cs_interpretation_layer_present('8c34ccd2-d33a-4dbe-940f-451537310766').
narrative_ontology:cs_reading_relation('8c34ccd2-d33a-4dbe-940f-451537310766', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c34ccd2-d33a-4dbe-940f-451537310766', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('8c34ccd2-d33a-4dbe-940f-451537310766', foundational, maori_text_binding).
narrative_ontology:cs_axiom_status(maori_text_binding, holdable).
narrative_ontology:cs_axiom_grounding('8c34ccd2-d33a-4dbe-940f-451537310766', maori_text_binding, deontological).
narrative_ontology:cs_axiom('8c34ccd2-d33a-4dbe-940f-451537310766', foundational, inherent_sovereignty_retained).
narrative_ontology:cs_axiom_status(inherent_sovereignty_retained, holdable).
narrative_ontology:cs_axiom_grounding('8c34ccd2-d33a-4dbe-940f-451537310766', inherent_sovereignty_retained, deontological).
narrative_ontology:cs_reference_frame('8c34ccd2-d33a-4dbe-940f-451537310766', article_ii_literal_construction).
narrative_ontology:cs_drift_state('8c34ccd2-d33a-4dbe-940f-451537310766', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8c34ccd2-d33a-4dbe-940f-451537310766', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_collective).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_statutory_authority).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_land_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, treaty_settlement_beneficiaries).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, indigenous_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, non_cession_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, Māori iwi retain inherent authority (tino rangatiratanga) over ancestral lands, natural resources, and taonga (treasures) via the Māori-language text of Article II. They administer customary governance structures, manage resource allocation according to tikanga (customary law), and claim co-governance or independent authority over conservation, fisheries, and land use within their rohe (territories). Their reading of the Treaty text is their primary instrument of claim; exit is identity-constitutive (Māori identity is territorially and genealogically bound to specific rohe and resource relationships).
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_collective, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_collective, beneficiary).

% Holds formal statutory authority over land titles, resource licensing, and conservation management through the Resource Management Act, Conservation Act, and Fisheries Act. Under the rangatiratanga reading, this authority is in structural conflict with Māori claims — the Crown's legislative sovereignty is seen as having limited itself to kāwanatanga (non-Māori governance, or 'governorship' of settlers/non-Māori affairs) while ceding tino rangatiratanga to Māori. The Crown's exit would require constitutional amendment or surrender of statutory control — highly constrained. It administers the constraint through legislation, judicial defense, and Crown funding of its own counsel in Treaty disputes.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_statutory_authority, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_statutory_authority, agenda_setter).

% Hold or claim Crown title to land historically Māori-occupied. Under this reading, their title derives from Crown authority to allocate non-Māori jurisdiction only (kāwanatanga), not from Crown ability to cede Māori land rights. Rangatiratanga claims place settler title in jeopardy; they pay via legal costs defending title, potential land loss through settlement, or acceptance of co-governance constraints. Exit is constrained by property law and by geography (cannot relocate their land).
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_land_claimants, payer,
    moderate, biographical, constrained, regional).

% Iwi who have reached direct settlements with the Crown in the post-1975 settlement process. They receive financial redress, returned Crown land, and co-governance or consultation protocols. The rangatiratanga reading legitimates their claim to ongoing authority rather than one-time compensation. Their benefit flows from the Crown's acceptance (partial, contested) of the rangatiratanga principle in settlement negotiations. Exit is identity-constitutive (settlement status is integral to iwi sovereignty claims and resource relationship).
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, treaty_settlement_beneficiaries, beneficiary,
    organized, generational, identity_locked, national).

% The institutional and statutory machinery through which the Crown enforces its reading of sovereignty — primarily the Resource Management Act, Conservation Act, Fisheries Act, Public Finance Act, and the common-law doctrine of Crown prerogative. Not an agent, but the framework that embeds one reading and resists the competing reading. Maintained through judicial defense and legislative amendment.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_legal_framework, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_legal_framework).

% Interprets the Treaty and applies it to disputes. The judiciary has slowly shifted toward acknowledging rangatiratanga as a live reading (e.g., Waitangi Tribunal reports, Court of Appeal findings), but common-law supremacy doctrine and reliance on Crown statute still privilege the Crown's reading. Judicial review of administrative action is the primary institutional lever by which the rangatiratanga reading can constrain Crown authority.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_judiciary, observer,
    institutional, generational, analytical, national).

% The body established (1975) to hear claims about Crown breaches of the Treaty. It has consistently found that the Crown breached the Treaty by ignoring Māori rights to land and resources and by failing to recognize tino rangatiratanga. Its findings are advisory — the Crown can ignore them — but they carry moral and political weight and have influenced settlement negotiations. Institutional position: recommend remedies but cannot enforce them against Crown resistance.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Advocates for the crown_sovereignty_reading (English text precedence, parliamentary supremacy) or the partnership_reading (ongoing consultation without structural authority transfer). These parties are structurally outside the rangatiratanga frame — they dispute that Article II's language constrains Crown authority at all. Their exclusion is not formal (they can participate in litigation and politics) but structural: the rangatiratanga reading's core premise (Māori language text carried binding force) contradicts their premise (Crown ceded complete sovereignty). They are present in litigation and politics but not in the rangatiratanga institutional structure itself.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, rival_sovereignty_reading_advocates, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_statutory_authority).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Treaty of Waitangi text establishes a framework for lawful coexistence between Māori and British settlers. The rangatiratanga reading solves the coordination problem of determining authority boundaries: who governs whom, whose law applies where, who controls resource allocation. By reading Article II's Māori text as binding, this reading coordinates two sovereign peoples under a division of powers — tino rangatiratanga over Māori territories and resources, kāwanatanga (limited to settler/non-Māori governance) for the Crown.
% TRANSFER_FUNCTION: Transfers legitimacy and de facto authority from Crown statutory supremacy to iwi-based governance over land, resources, and taonga. Operationally: Māori settlements receive Crown-funded redress, co-governance seats on resource-management bodies, and recognition of customary rights in law; Crown retains formal title but cedes decision authority; settler land claimants lose security of title or face constraints on land use.
% ABSENT_VOICES: Rival sovereignty readings (Crown supremacy advocates, partnership-reading proponents) are formally present in litigation and politics but structurally absent from governance structures built on the rangatiratanga frame. Settlers without indigenous claim, dependent on Crown-allocated land, would object to rangatiratanga recognition but have limited political voice. Smaller iwi who have not yet secured settlements are partially excluded — they benefit from the reading's vindication but lack the resources of settled iwi.
% DISAPPEARANCE_RATIONALE: If the rangatiratanga reading were decisively rejected and Crown sovereignty asserted absolute, existing co-governance arrangements (e.g., Fisheries Act provisions, Conservation Act protocols, settlement co-management of Ngāi Tahu and Tainui lands) would collapse. Iwi authority over forests, fisheries, and conservation would revert to Crown statutory control. Settlements would be legally reinterpreted as one-time transfers with no ongoing authority claim. The political basis for land return and co-governance would evaporate. The world rearranges toward Crown administrative supremacy.
% FOUNDING_PROBLEM: At the time of Treaty signing (1840), Britain sought to legitimize settler colonization by obtaining Māori consent through a formal treaty. Māori understood themselves to be guaranteeing a division of authority: they would accept British-governed settlement in exchange for Crown recognition of their continuing authority over their own rohe, people, and resources. The rangatiratanga reading holds that this founding problem — legitimate coexistence under divided sovereignty — was structurally solved by the text, with Article II retaining Māori authority.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal (1975–present) has consistently found the Crown breached the founding problem solution by ignoring Māori rangatiratanga rights and unilaterally alienating Māori lands. Court of Appeal judgments (e.g., Ngāi Tahu v. Director-General, Taiaroa v. Minister of Justice) acknowledge rangatiratanga as a live Treaty principle. Independent historical scholarship (e.g., Claudia Orange, Paul McHugh) attests that Māori signatories understood the Māori text as guaranteeing retained authority. Crown itself (in settlement negotiations and legislative amendments like the Resource Management Act 1991 amendments) has partially conceded the principle, though it disputes the scope. The reading is corroborated by institutions outside the beneficiary seats — tribunal, courts, historians — but the Crown (primary payer) disputes both the problem statement and the solution.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).
:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The rangatiratanga reading describes a constraint that TRANSFERS authority (from Crown to Māori) rather than pure extraction, which is why extractiveness is lower than suppression. At time 0 (1840), the constraint was highly extractive (0.78): Crown seized Māori lands, suppressed Māori governance, and enforced that suppression heavily (0.89). The constraint's justification was theatrical from the start — the Crown claimed to honor the Treaty while unilaterally interpreting 'cession' as total sovereignty, contradicting the Māori text. Theater was low (0.15) because the suppression was direct and overt (land confiscation, military enforcement). Over time (interval 0–184 years, roughly 1840–2024), extractiveness declined as the rangatiratanga reading gained institutional recognition (Tribunal, courts, settlements). By interval end, the constraint is less extractive (0.31) because Māori have recovered some decision authority via co-governance and settlements. But suppression has remained relatively high (0.67) because the Crown still resists full authority transfer and maintains statutory control. Theater has risen dramatically (0.15 → 0.72) because enforcement shifted from overt taking to consultation frameworks that appear to honor Māori voice but retain Crown veto power (consultation without binding effect, co-governance seats without decision authority). This is the Piton drift pathway: the constraint's primary function (legitimizing settler land access) has been undermined by the rangatiratanga reading's institutional victories, so the Crown now performs consultation and settlement to maintain the appearance of compliance while preserving statutory supremacy. Measurement series: extractiveness and suppression_requirement decline together (as rangatiratanga recognition grows); theater rises as the Crown substitutes performative compliance for direct suppression. One shared time grid (all metrics authored at every examined time point) so comparative drift analysis is valid.
 *
 * PERSPECTIVAL GAP:
 *   See above — the seats diverge on whether the constraint is a betrayal (Māori reading) or a partnership-in-progress (Crown reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each seat derives from beneficiary/victim declarations and exit options. Māori iwi are declared beneficiaries (the reading vindicates their authority claim) and are the agenda-setters (they assert the reading and leverage settlements). Their exit is identity_locked (Māori identity is inseparable from territorial and genealogical relationships; 'exiting' Māori identity is not a real exit). Consequently d is near the beneficiary end (low d, subsidized by the constraint), with subsidization bounded by the constraint's incompleteness (they benefit from recognition but lack full realized authority, so d rises above pure beneficiary toward moderate). Crown statutory authority is declared a victim (resists the reading, loses authority) and is the constrained agenda-setter (defends Crown supremacy through statute and litigation). Exit is constrained (constitutional amendment required to surrender sovereignty; politically impossible). Consequently d is near the target end (high d, extracted from). Settler land claimants are declared victims (title at risk); exit is constrained (geography, property law). Consequently d is high. No directionality override is needed — the derivation chain (beneficiary/victim + exit) produces accurate d values. The reading's institutional victories shift d downward for the Crown and upward (less extraction, more subsidy) for Māori, reflecting the historical trajectory.
 *
 * MANDATROPHY ANALYSIS:
 *   The rangatiratanga reading does NOT face mandatrophy because its founding problem (divided sovereignty, legitimate coexistence) remains live and contested. The Tribunal and courts have consistently affirmed that the Crown breached this problem-solving by ignoring Article II; the founding problem has not been solved and abandoned. What HAS shifted is FUNCTION: the original function (legitimizing settler takeover) has been corrupted, and the constraint now operates increasingly through theater (consultation without power) rather than direct extraction (land taking). This is the Piton drift pattern: the founding function is dead (Crown cannot openly claim unilateral sovereignty anymore), but the constraint persists through performative maintenance (settlement negotiations, consultation protocols, co-governance with Crown veto). Mandatrophy would apply if the rangatiratanga principle were fully realized and Māori authority were actually transferred — then the constraint's function would be satisfied and it would be abandoned. Currently: founding problem is still live (Māori authority not realized), function is degraded to theater, and the constraint persists by institutional inertia and Crown resistance. Not quite mandatrophy, but approaching it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_ii_binding_force,
    'Does the Māori-language text of Article II have independent binding force as an international treaty provision, or does the English text (Article I) control as the official version?',
    'International law principles (Vienna Convention on the Law of Treaties, interpretive rules for ambiguous treaty texts, good-faith interpretation) would favor the reading that is most favorable to the indigenous party in case of ambiguity. Courts would need to formally adopt this rule; currently they treat the texts as equally authoritative but interpretively favor the English text via common-law supremacy.',
    'If Article II is formally recognized as binding and controlling for disputes about Māori rights, the rangatiratanga reading becomes legally crystallized and the Crown''s ability to defend statutory supremacy collapses. If English text controls, the rangatiratanga reading remains a political and moral claim without legal force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_ii_binding_force, empirical, 'Whether the Māori-language Treaty text is binding in law or merely advisory/moral.').

omega_variable(
    tino_rangatiratanga_scope,
    'What does tino rangatiratanga actually entail: absolute Māori sovereignty over traditional territories (requiring Crown withdrawal from those territories), or Māori decision-making authority within Crown-retained framework (co-governance, veto rights, resource management authority)?',
    'Court judgments, settlement agreements, and legislation would define the scope through accumulated practice. The Waitangi Tribunal and settlement negotiations have iterated on definitions (e.g., co-management of fisheries, co-governance of Ngāi Tahu lands under the Ngāi Tahu Claims Settlement Act); these provide evidence of scope but remain contested.',
    'A maximal scope (absolute sovereignty, Crown withdrawal) would make the constraint structurally incompatible with Crown statehood and would require partition or fundamental constitutional restructuring. A minimal scope (consultation rights, cultural recognition) would make the constraint compatible with Crown supremacy, reducing extractiveness sharply. The rangatiratanga reading''s viability depends on a mid-range scope being realized (meaningful decision authority, Māori veto on resource use in traditional territories, co-governance structures with binding effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tino_rangatiratanga_scope, conceptual, 'Whether tino rangatiratanga means absolute sovereignty, co-governance with binding Māori authority, or consultation rights within Crown framework.').

omega_variable(
    suppression_internalization,
    'To what degree has the suppression of the rangatiratanga reading been internalized by Māori themselves through colonial education, institutional exclusion, and identity fragmentation, versus remaining structural (external legal barriers, Crown monopoly on resource licensing)?',
    'Post-settlement recovery trajectories: Do iwi who have received Crown recognition and co-governance seats show increased capacity for independent authority assertion? Or do they continue to defer to Crown frameworks even when legally empowered? Comparative analysis of settlement outcomes and iwi governance capacity over time.',
    'If suppression is largely internalized (Māori have internalized Crown supremacy as legitimate), then even legal recognition will not immediately produce independent authority — there is a recovery lag and ongoing psychological/institutional decolonization required. If suppression is primarily structural (Crown legal barriers), then removing barriers (legislating co-governance, recognizing customary authority) produces rapid authority shift. The distinction affects the timeline and mechanisms of the rangatiratanga reading''s realization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of rangatiratanga is structural or internalized, affecting recovery pace post-recognition.').

omega_variable(
    kernel_reading_contest,
    'Is the waitangi_sovereignty_allocation kernel actually indeterminate in its language (genuinely ambiguous, permitting multiple readings of Article II), or does the Māori text unambiguously carry the rangatiratanga meaning and the Crown''s crown_sovereignty_reading is an illegitimate reinterpretation?',
    'Historical linguistic and contextual analysis: Māori signatories'' understanding of tino rangatiratanga at the time of signing; Crown officials'' understanding of the gap between Article I and II; Māori-language scholarship on the terms. If Māori signatories clearly understood Article II as retaining authority and Crown officials also knew this (and proceeded anyway), the kernel is not genuinely ambiguous — the Crown reading is an ex post facto reinterpretation.',
    'If the kernel is genuinely ambiguous, all three readings (crown_sovereignty, partnership, rangatiratanga) are live and the constraint''s resolution remains open. If the kernel is unambiguous in favor of rangatiratanga, then crown_sovereignty is a illegitimate reading imposed by force, and the constraint''s resolution should converge on rangatiratanga. This affects whether the reading should be treated as one option among three (coexists_with) or as the correct reading being suppressed (influences/forecloses the others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether the Treaty kernel is genuinely ambiguous or unambiguously carries the rangatiratanga meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(wait_tr_t0, observed).
narrative_ontology:measurement(wait_tr_t23, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 23, 0.28).
narrative_ontology:measurement_basis(wait_tr_t23, observed).
narrative_ontology:measurement(wait_tr_t46, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 46, 0.42).
narrative_ontology:measurement_basis(wait_tr_t46, observed).
narrative_ontology:measurement(wait_tr_t92, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 92, 0.61).
narrative_ontology:measurement_basis(wait_tr_t92, observed).
narrative_ontology:measurement(wait_tr_t138, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 138, 0.68).
narrative_ontology:measurement_basis(wait_tr_t138, observed).
narrative_ontology:measurement(wait_tr_t184, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 184, 0.72).
narrative_ontology:measurement_basis(wait_tr_t184, observed).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement_basis(wait_be_t0, observed).
narrative_ontology:measurement(wait_be_t23, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 23, 0.71).
narrative_ontology:measurement_basis(wait_be_t23, observed).
narrative_ontology:measurement(wait_be_t46, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 46, 0.59).
narrative_ontology:measurement_basis(wait_be_t46, observed).
narrative_ontology:measurement(wait_be_t92, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 92, 0.45).
narrative_ontology:measurement_basis(wait_be_t92, observed).
narrative_ontology:measurement(wait_be_t138, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 138, 0.38).
narrative_ontology:measurement_basis(wait_be_t138, observed).
narrative_ontology:measurement(wait_be_t184, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 184, 0.31).
narrative_ontology:measurement_basis(wait_be_t184, observed).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0, 0.89).
narrative_ontology:measurement_basis(wait_su_t0, observed).
narrative_ontology:measurement(wait_su_t23, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 23, 0.85).
narrative_ontology:measurement_basis(wait_su_t23, observed).
narrative_ontology:measurement(wait_su_t46, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 46, 0.79).
narrative_ontology:measurement_basis(wait_su_t46, observed).
narrative_ontology:measurement(wait_su_t92, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 92, 0.71).
narrative_ontology:measurement_basis(wait_su_t92, observed).
narrative_ontology:measurement(wait_su_t138, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 138, 0.68).
narrative_ontology:measurement_basis(wait_su_t138, observed).
narrative_ontology:measurement(wait_su_t184, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 184, 0.67).
narrative_ontology:measurement_basis(wait_su_t184, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.18).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_resource_management_act__maori_participation_framework).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_fisheries_quota_management__maori_allocation).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_court_jurisdiction__native_title_recognition).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the waitangi_sovereignty_allocation kernel. The crown_sovereignty_reading (English text, complete cession) and partnership_reading (consultative relationship, no structural transfer) are structurally distinct constraints with their own ε values, beneficiary/victim structures, and classifications. The rangatiratanga_reading presented here coexists with the crown_sovereignty_reading in the same jurisdiction and time period — the Crown defends crown_sovereignty through statute while courts and Tribunal increasingly validate rangatiratanga. The partnership_reading influences both by providing a middle path that avoids both complete Crown supremacy and full Māori sovereignty. All three are necessary to model the kernel contest; no single constraint captures the full picture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
