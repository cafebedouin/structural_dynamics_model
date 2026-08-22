% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate Authority (Public Health Primary Reading)
 *   domain: public_health/constitutional/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the PUBLIC_HEALTH_PRIMARY reading of
 *   the public_health_mandate_authority kernel. It frames public health
 *   mandate as a legitimate state obligation to protect vulnerable
 *   populations (immunocompromised, healthcare infrastructure) through
 *   collective action, enforceable via employment conditions, service access,
 *   and legal coercion. The reading treats vaccine hesitant and
 *   mandate-resistant individuals as free-riders imposing externalities on
 *   the vulnerable. Immunocompromised populations are identified as the
 *   primary beneficiaries of the mandate; mandate-resistant workers are the
 *   structural targets of extraction (employment loss, service exclusion,
 *   coercive vaccination). The constraint is classified as TANGLED ROPE
 *   because it combines genuine coordination (protecting the vulnerable) with
 *   asymmetric extraction (targeting the resistant). This reading does NOT
 *   claim bodily autonomy is unimportant; it subordinates autonomy claims to
 *   the collective-protection axiom: when the vulnerable face severe harm and
 *   alternatives are exhausted, the state's duty to protect overrides
 *   individual medical sovereignty.
 *
 * KEY AGENTS:
 *   - immunocompromised_populations: vulnerable beneficiaries of mandate; at measurable risk if unvaccinated prevalence remains high; protected by mandate enforcement
 *   - healthcare_system_integrity: institutional beneficiary; mandate reduces transmission to healthcare workers, maintains surge capacity, stabilizes medical infrastructure
 *   - mandate_resistant_workers: primary targets of extraction; face employment loss, service exclusion, social sanction; coerced into vaccination via negative incentives
 *   - vaccine_hesitant_individuals: secondary target population; face social and institutional pressure; framed as free-riders imposing externality on vulnerable
 *   - public_health_authority: agenda-setter; designs, enforces, and adjudicates mandate; authorized by public health law and constitutional emergency powers
 *   - rights_advocacy_organizations: excluded voices; would object to mandate extraction and frame it as rights violation; structurally absent from health authority deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.68).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.71).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate Authority (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health/constitutional/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, 'f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a').
narrative_ontology:cs_kernel_codification('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', formalized).
narrative_ontology:cs_authority_grounding('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', extraction).
narrative_ontology:cs_interpretation_layer_present('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a').
narrative_ontology:cs_reading_relation('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', foundational, collective_protection_overrides_autonomy).
narrative_ontology:cs_axiom_status(collective_protection_overrides_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', collective_protection_overrides_autonomy, deontological).
narrative_ontology:cs_axiom('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', secondary, externality_justifies_coercion).
narrative_ontology:cs_axiom_status(externality_justifies_coercion, holdable).
narrative_ontology:cs_axiom_grounding('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', externality_justifies_coercion, empirically_contingent).
narrative_ontology:cs_reference_frame('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', public_health_mandate_authority_primary).
narrative_ontology:cs_drift_state('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', contemporary_endemic_phase, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f5d7c1b6-9e81-4a91-abfa-3e57ae6a565a', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_system_integrity).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_workers).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, vaccine_hesitant_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, vaccine_hesitant_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Comprise individuals with severe immune suppression (organ transplant recipients, advanced HIV/AIDS, certain cancer patients, primary immunodeficiencies). They cannot rely on their own immune response for protection and depend on collective vaccination of others to prevent exposure to pathogens. They face severe clinical risk if unvaccinated populations maintain high transmission; hospitalization can be fatal. They cannot exit the constraint—they cannot move to a jurisdiction where mandate-resistant populations do not impose externalities on them. They receive direct protective benefit from mandate enforcement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, global).

% The institutional infrastructure that provides medical care. Healthcare system integrity is threatened by (1) transmission to healthcare workers, which reduces staffing and creates workforce absenteeism; (2) surge demand from unvaccinated sick patients overwhelming ICU and hospital capacity; (3) reputational and operational pressure from simultaneous pandemic response and routine care. The mandate reduces transmission to healthcare workers and lowers surge demand, stabilizing the system. Healthcare systems benefit from mandate enforcement by maintaining operational capacity and staff availability.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_system_integrity, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_non_agent(public_health_mandate_authority__public_health_primary, healthcare_system_integrity).

% Individuals actively opposed to vaccination mandates on grounds of bodily autonomy, medical skepticism, distrust of pharmaceutical companies, or ideological objection to state medical authority. Many are employed in healthcare, education, or public-facing sectors where mandates are enforced most strictly. They face employment loss, professional license suspension, service exclusion (airline travel, hospital entry, restaurant access in some jurisdictions), and social sanction. Their exit options are constrained: changing professions requires retraining; leaving the country is available to few; accepting vaccination is experientially equivalent to defeat (identity-locked exit). The constraint extracts employment and social standing from this group to fund collective protection.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_workers, payer,
    moderate, biographical, constrained, national).

% Individuals who are not actively opposed to vaccination but are uncertain about safety, efficacy, or necessity. They face institutional pressure to vaccinate (employment, education access, service restrictions in some jurisdictions) but lack the strong ideological commitment to resist that mandate-resistant workers possess. Their hesitation can be overcome by access to trusted information, peer example, or direct health advice—they occupy the margin where extraction (coercive pressure) can induce compliance. Some vaccine-hesitant individuals remain vaccine-hesitant due to identity-fusion with 'vaccine-skeptical' social networks, which makes exit costly even when the barriers are removed.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, vaccine_hesitant_individuals, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__public_health_primary, vaccine_hesitant_individuals, beneficiary).

% Government public health departments and officials who design, implement, and enforce vaccination mandates. They exercise authority derived from public health law (emergency powers, disease control statutes) and constitutional provisions (state police power). They set the mandate conditions (who must vaccinate, which exemptions apply, what penalties attach), enforce them via employment and service restrictions, and adjudicate disputes. They benefit from mandate compliance by reducing disease transmission and maintaining healthcare system capacity. They claim authority to enforce mandates on grounds that collective protection overrides individual medical autonomy; they frame resistance as free-riding on the benefits of others' vaccination. Their exit options are political and legal: they can relax mandates under political pressure, modify mandate scope to narrow targets, or litigate to defend mandate authority.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Civil liberties and rights organizations that oppose public health mandates on grounds that they violate bodily autonomy, due process, and medical privacy. They have substantial legal resources and political voice (litigation, legislative advocacy, media campaigns) but are structurally excluded from the public health authority's decision-making process. They are not parties to mandate design or enforcement; their objections enter the constraint's operation only as external pressure (litigation risk, political cost, media criticism). If included in mandate deliberation, they would argue for narrower mandates, robust exemption processes, and proportionality assessment—their inclusion would substantially alter the constraint's extractiveness and enforcement intensity.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, rights_advocacy_organizations, excluded,
    powerful, generational, mobile, national).

% An external analytical seat that assesses the constraint's classification, extractiveness, and legitimacy from no partisan position. The observer examines whether the constraint's operation (protecting the vulnerable, extracting from the resistant) aligns with its stated justification (collective protection) and whether the measured extraction is proportional to the protection benefit.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__public_health_primary, public_health_authority).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieve sufficient population vaccination (or acquired immunity) to reduce transmission to levels where immunocompromised and vulnerable populations face manageable risk, and to prevent healthcare system collapse during surge periods. The coordination problem: individual vaccination decisions create positive externalities (protection for others) that individuals do not capture in their own cost-benefit calculation; absent coordination, individuals undervaccinate from the collective perspective. Mandates solve this by internalizing the externality—vaccination becomes a condition of employment or service access, making the collective benefit a private cost for non-compliance.
% TRANSFER_FUNCTION: Moves employment, social standing, healthcare access, and bodily autonomy from mandate-resistant and vaccine-hesitant populations to the public health authority and indirectly to vulnerable populations (who receive protection). The transfer is asymmetric: immunocompromised and healthcare systems receive direct protection benefit; resistant and hesitant populations bear direct extraction costs (employment loss, service exclusion, coerced medical intervention). The transfer mechanism is enforcement: legal penalties, administrative exclusion, and social sanction compel vaccination compliance.
% ABSENT_VOICES: Civil liberties organizations, vaccine-skeptical communities, and mandate-resistant workers are substantially excluded from mandate design deliberation. These voices would argue for narrower mandates (targeting only high-risk settings), robust medical and religious exemptions, and proportionality assessment tied to threat severity. Their absence is structural: public health authorities design mandates through scientific and epidemiological deliberation; rights and liberty objections are treated as external political pressure rather than inputs to mandate design. If these voices were seated, mandate scope would likely narrow and extraction would decline (fewer targets, more exemptions).
% DISAPPEARANCE_RATIONALE: If the public health mandate suddenly disappeared, the world would rearrange substantially: (1) Unvaccinated populations would increase significantly, raising transmission risk for immunocompromised individuals, who would face either isolation or increased clinical risk. (2) Healthcare workers could leave employment without vaccine condition, potentially reducing workforce stability and surge capacity. (3) Social risk for vulnerable populations would rise acutely. (4) Mandate-resistant individuals would gain employment and service access, reintegrating into labor and social markets. The constraint's disappearance would alter the equilibrium substantially for multiple parties; it is not a marginal arrangement.
% FOUNDING_PROBLEM: Severe acute respiratory illness (COVID-19 as the referent crisis, though the constraint generalizes to future pandemics) threatens healthcare system capacity and vulnerable populations. Individual vaccination choices create externalities: unvaccinated individuals are more likely to contract and transmit infection, raising risk for others. Without collective action, individuals have insufficient incentive to vaccinate; they face personal vaccine risk and benefit individually from others' vaccination, but bear no cost for imposing risk on others. The state intervenes to align private incentives with collective welfare by making vaccination a condition of employment and service access, internalizing the externality.
% FOUNDING_PROBLEM_CORROBORATION: PUBLIC HEALTH AUTHORITY ATTESTATION: Public health departments and epidemiologists attest the problem is live and ongoing. Vulnerable immunocompromised populations face documented clinical risk from circulating pathogens; healthcare system capacity remains a constraint during surge periods; transmission from unvaccinated individuals is measurable. INDEPENDENT CORROBORATION: Epidemiological studies from academic medical centers document excess mortality and morbidity in immunocompromised populations during high-transmission periods; healthcare worker surveys document workforce impacts during surge demand. COUNTER-ATTESTATION (from excluded voice): Rights advocates and vaccine-skeptical communities attest that the founding problem is substantially resolved: disease mortality has declined substantially, treatments are available, vulnerable populations can protect themselves via prophylaxis and isolation rather than requiring collective vaccination. Independent economic analysis suggests pandemic economic productivity has recovered, reducing healthcare system emergency claims. NO UNANIMOUS CORROBORATION: The founding problem status is genuinely contested. Parties sympathetic to public health authority (epidemiologists, healthcare leadership, immunocompromised patient advocates) attest the problem is live. Parties skeptical of mandate legitimacy attest the problem has substantially resolved.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply from 0.35 to 0.68 over the 30-unit interval, with acceleration in early phase (0–12) as enforcement machinery scales up and employment consequences materialize. This trajectory reflects the constraint's temporal logic: initial mandate announcement (low extraction, high voluntary compliance) → enforcement ramp (employment conditions, service restrictions materialize) → enforcement plateau (extraction reaches steady-state where remaining targets are most constrained). Suppression rises from 0.42 to 0.71 in parallel, reflecting the enforcement machinery's maturation: legal penalties, regulatory exclusions, and social stigma accumulate. Theater ratio remains low (0.28 at end) because the constraint's function is substantially real (protecting immunocompromised, maintaining healthcare capacity), not primarily theatrical—enforcement defends actual coordination, not performative compliance. However, theater rises over time as the protective function (early phase, genuine public health work) is increasingly defended via symbolic enforcement (employment termination, professional licensing restrictions) that may exceed marginal protection gain. Accessibility collapse (0.62) is moderate: alternatives (masking, testing, remote work, prophylaxis) persist and are not fully suppressed, but the mandate narrows choices enough that vaccine compliance becomes the default path. Resistance (0.74) is substantial: mandate-resistant populations actively resist through legal challenge, workplace litigation, political organization, and non-compliance where penalties allow. The constraint persists despite high resistance because the agenda-setter (public health authority, backed by state power) has enforcement capacity that exceeds resistance capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority seat, the constraint is legitimate coordination: the vulnerable need protection, the authority exists to provide it, resistance is irrational free-riding on collective protection. From the mandate-resistant seat, the constraint is pure extraction: the authority uses disease protection as rhetorical cover for bodily coercion, the 'vulnerable protection' claim is overstated, and resistance is defense of medical autonomy. From the immunocompromised seat, the constraint is unambiguous benefit: they face severe risk, the mandate reduces that risk substantially, and the cost borne by resisters is acceptable as the price of their protection. From a rights-advocacy observer seat, the constraint is a rights violation disguised as public health: it violates bodily autonomy, uses group vulnerability as justification for individual coercion, and sets precedent for expanding state medical authority. These perspectives compute to different types from identical structural data because directionality differs per seat: the engine computes per-seat classification from beneficiary/victim declarations and exit options. The constraint is simultaneously legitimate collective action (immunocompromised perspective, public health authority perspective) and illegitimate extraction (rights-advocate perspective, resistant perspective). This is not contradiction; it is the normal multi-seat computation of a constraint whose benefits and costs are asymmetrically distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply across seats. Immunocompromised populations experience low d (near 0.1–0.2): they are net beneficiaries, have constrained exit (cannot isolate indefinitely, require healthcare access), and benefit substantially from reduced transmission. Healthcare system experiences d near 0.15: it coordinates around reduced transmission and maintains capacity, with minimal extraction. Mandate-resistant workers experience high d (near 0.85–0.95): they are the extraction targets, face employment loss as the enforcement mechanism, and have trapped or identity-locked exit (many lack retraining options, ideological investment in resistance hardens over time). Vaccine-hesitant individuals experience d near 0.75: they are secondary targets, face social sanction and institutional barriers, but retain more choice than the actively resistant. Public health authority experiences moderate d near 0.4–0.5: it sets the rules and benefits from mandate compliance, but also bears compliance costs (monitoring, enforcement, political friction) that prevent pure beneficiary positioning. The divergence is structural: the constraint coordinates for some (immunocompromised, healthcare) and extracts from others (resistant) through the same mechanism. This asymmetry is the signature of TANGLED ROPE.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids MANDATROPHY because the founding problem remains contested and partially live. The founding problem: severe acute respiratory illness threatens healthcare capacity and vulnerable populations; individuals' vaccination choices create externalities; collective action is required. FOUNDING_PROBLEM_STATUS in six_questions is CONTESTED because: (1) The threat severity is disputed: some parties assess COVID-19 as substantially endemic with manageable mortality (problem status: dead); public health authorities assess ongoing transmission as requiring sustained protective action (problem status: live). (2) The externality mechanism is contested: resisters argue vaccination does not substantially reduce transmission to others (externality claim: dead); health authorities argue reduced transmission and hospitalization justify mandates (externality claim: live). Because the founding problem status is contested rather than clearly dead, and the disappearance verdict is world_rearranges (healthcare and vulnerable populations would lose protection), mandatrophy has not resolved—the mandate persists because the founding problem remains partially live, not because it is a zombie arrangement. However, the omega variables document the contestation: if the threat severity is genuinely low and alternatives are available, the mandatrophy_resolved flag should be set true (in future assessment, not this story) and the constraint reclassified as PITON. The measurement series show extraction accumulation over time (base_extractiveness 0.35→0.68), which T17 would flag as mandatrophy candidate; the offset is that suppression also rises (0.42→0.71), tracking increased enforcement rather than degraded function. This is consistent with a constraint whose function (protecting the vulnerable) persists but whose extraction cost rises as enforcement machinery matures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_bodily_autonomy,
    'Is bodily sovereignty categorically inviolable, or can collective protection of the vulnerable override individual medical autonomy under specified conditions?',
    'This reading (public_health_primary) asserts that collective protection can override autonomy when externalities are severe and alternatives exhausted. The bodily_autonomy_primary reading asserts categorical immunity. The empirical resolution hinges on: (1) whether unvaccinated individuals do impose measurable harm on the immunocompromised (causal chain: exposure → transmission → clinical harm), and (2) whether less coercive alternatives (testing, masking, quarantine) achieve equivalent protection without bodily intervention.',
    'If externalities are severe and alternatives fail, the collective-protection axiom (public_health_primary) holds and mandates are legitimate. If externalities are minimal or alternatives work, the axiom fails and the bodily_autonomy_primary reading forecloses this one. The boundary is empirical-but-normative: causal facts (harm magnitude, alternative efficacy) determine which axiom survives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_bodily_autonomy, empirical, 'Whether unvaccinated populations impose measurable externalities on the immunocompromised, and whether less coercive alternatives achieve equivalent protection.').

omega_variable(
    kernel_contest_proportionality,
    'Does mandate legitimacy require proportionality assessment (threat severity × coercion magnitude × alternative availability × duration), or does the public-health axiom grant state authority to mandate regardless of proportionality?',
    'The proportionality_reading asserts that sliding-scale assessment is mandatory: high threat + minimal alternatives + time-limited + low coercion = legitimacy; low threat + high alternatives + indefinite + severe coercion = illegitimacy. This reading (public_health_primary) subordinates proportionality to the collective-protection axiom: if the vulnerable are protected, the mandate is justified. The readings coexist as live positions—different authority traditions (public health law vs. rights jurisprudence) assign different weights to proportionality.',
    'A mandate claimed legitimate under public_health_primary (protects immunocompromised) might fail proportionality_reading assessment (low threat, high alternatives, severe employment coercion, indefinite duration). The two readings operate on non-overlapping authority frames and produce different verdicts on identical facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_proportionality, conceptual, 'Whether proportionality assessment is binding on mandate authority or subordinate to collective-protection duty.').

omega_variable(
    identification_of_vulnerability,
    'Who counts as ''vulnerable commons'' in the immunocompromised set, and under what conditions does vulnerability create extraction rights against the hesitant?',
    'This reading identifies immunocompromised as the primary vulnerable set entitled to protection via mandate. But immunocompromise is not binary: degrees of immune suppression, availability of alternative protections (prophylaxis, monoclonal antibodies, isolation), and baseline risk vary widely. A narrow reading (severe combined immunodeficiency, zero alternatives) vs. broad reading (mild immune suppression, many alternatives) produces different victim sets and different extraction entitlements on the resistant. The vulnerability boundary is where mandate-extracted cost shifts from justified protection to rent collection.',
    'A narrow vulnerability reading narrows the victim set, contracts the mandate''s legitimacy scope, and reclassifies some mandate-forced costs as pure extraction (snare). A broad reading expands the victim set, expands the mandate scope, and defends more coercion as justified collective protection (tangled_rope). The boundary determination is where this reading''s extractiveness is most contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identification_of_vulnerability, empirical, 'The scope and specificity of immunocompromise conditions that trigger mandate protection obligations.').

omega_variable(
    free_rider_vs_rights_bearer,
    'Are mandate-resistant individuals best characterized as free-riders imposing negative externality (the public-health reading''s frame), or as rights-bearers exercising bodily autonomy that the state is illegally suppressing (the bodily-autonomy reading''s frame)?',
    'The two readings assign opposite roles to the same agent. This reading names them in the victim set (mandate-resistant_workers) — but victim here means ''targeted by extraction to fund collective protection,'' not ''wrongfully harmed.'' The bodily-autonomy reading would name them as rights-bearers harmed by the mandate itself, placing them in a different victim set relative to a different constraint (the constraint being the mandate authority itself, not the protective obligation). The role-assignment is a frame choice, not an empirical fact.',
    'If mandate-resistant are free-riders (public_health_primary frame), extraction from them is justified. If they are rights-bearers (bodily_autonomy_primary frame), the mandate is an illegitimate constraint and the ''extraction'' is a rights violation. The same behavioral fact (refusal to vaccinate) supports both frames; the frames are incommensurable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_rider_vs_rights_bearer, conceptual, 'The framing of vaccine-hesitant individuals as free-riders vs. rights-bearers.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) primarily structural (employment loss, service exclusion, legal coercion) or internalized (social stigma, identity fusion with mandate-resistance, perceived irrationality)?',
    'Post-mandate-removal trajectory: if suppression persists after legal coercion ends, reclassify as partially internalized. Measure via: survey of post-mandate employment/benefit access, social reintegration speed, and persistence of self-reported constraint experience among those who exit.',
    'If structural, the suppression ends with mandate removal. If internalized, the mandate-resistant carry the suppression forward; they are captured by the constraint''s legitimation narrative (the state ''protected the vulnerable, and resisters were irresponsible''). Internalized suppression indicates deeper cognitive capture and higher effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural (external coercion) or internalized (beliefs, identity, social stigma).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(publ_tr_t6, public_health_mandate_authority__public_health_primary, theater_ratio, 6, 0.12).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__public_health_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement(publ_tr_t18, public_health_mandate_authority__public_health_primary, theater_ratio, 18, 0.24).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__public_health_primary, theater_ratio, 24, 0.27).
narrative_ontology:measurement(publ_tr_t30, public_health_mandate_authority__public_health_primary, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(publ_be_t6, public_health_mandate_authority__public_health_primary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__public_health_primary, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(publ_be_t18, public_health_mandate_authority__public_health_primary, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__public_health_primary, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(publ_be_t30, public_health_mandate_authority__public_health_primary, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(publ_su_t6, public_health_mandate_authority__public_health_primary, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__public_health_primary, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(publ_su_t18, public_health_mandate_authority__public_health_primary, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__public_health_primary, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(publ_su_t30, public_health_mandate_authority__public_health_primary, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(public_health_mandate_authority__public_health_primary, 0.12).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__bodily_autonomy_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__public_health_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the public_health_mandate_authority kernel. All three readings share the same contesting domain (state mandate legitimacy) but instantiate different constraints because they assign different ε values to the mandate authority's operation. Public_health_primary reads high extraction (0.68) from the constraint's asymmetric burden on the resistant. Bodily_autonomy_primary would read the constraint itself (mandate authority) as categorically illegitimate extraction (ε near 1.0) from rights-bearers. Proportionality_reading would read ε as dependent on assessed proportionality (ε could range 0.2–0.9 depending on threat/coercion/alternative assessment). The three readings are linked via network.affects_constraints because the public_health_primary reading's legitimacy claims directly pressure the other two: if public_health_primary's axiom is correct (collective protection overrides autonomy), bodily_autonomy_primary is foreclosed; if proportionality_reading's axiom is correct (sliding-scale assessment is mandatory), public_health_primary overstates mandate legitimacy. See cs_structure.reading_relations for the relation types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
