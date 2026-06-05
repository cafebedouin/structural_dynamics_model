% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Thinkability Reading (1710 Statutory Coherence)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   In 1710, the English Statute of Anne created the first statutory
 *   codification of 'copy right' — transforming dispersed guild privilege,
 *   common-law precedent, and Crown grants into a unified legal category with
 *   explicit duration, transferability, and registration. This constraint
 *   story models ONE reading of that moment: the **thinkability reading**,
 *   which emphasizes that the statute's primary structural effect was to make
 *   ownable expression **thinkable** as a distinct legal kind. Before 1710,
 *   disputes over book production and circulation lacked the vocabulary and
 *   conceptual apparatus to frame disagreements as property claims. Printers
 *   invoked guild privilege, royal grant, or common-law precedent—each with
 *   different logic and scope. The statute provided coherence: a unified
 *   category (copy right) distinguished from guild monopoly, from patent,
 *   from real property, with its own duration rule and public reversion
 *   clause. This reading does not claim the statute **created** the
 *   underlying right—only that it created the conceptual space in which such
 *   rights became legible, enforceable, and tradeable. The statute enabled
 *   extraction by standardizing suppression (uniform licensing rules,
 *   registration enforcement, statutory damages) and beneficiary protection
 *   (property transfer, inheritance, duration certainty). But the statute
 *   also claimed to coordinate: by defining duration as finite and reversion
 *   as public, it framed the monopoly as temporary incentive rather than
 *   perpetual control. The thinkability reading emphasizes the **category
 *   emergence itself** as the binding mechanism—once 'copy right' is
 *   coherent, both beneficiaries and victims are locked into its logic.
 *   Beneficiaries (stationers, proprietors) gain enforceable property claims.
 *   Victims (unauthorized printers, the eventual public commons) lose access
 *   to the vocabulary for contesting the regime—all disputes must now happen
 *   within the copy-right frame.
 *
 * KEY AGENTS:
 *   - Stationers Company / Book Proprietors: Primary beneficiary (institutional/arbitrage) — transforms distributed guild privilege into individual property claim; gains statutory recognition and enforcement apparatus
 *   - Unauthorized Printers / Independent Producers: Primary victim (powerless/trapped in pre-1710 frame; constrained post-1710) — cannot operate without license; lack vocabulary to contest regime as illegitimate (suppressed by category coherence)
 *   - Parliament / Crown: Institutional authority (institutional/arbitrage) — creates regulatory framework; gains revenue and political control over printing; has full exit options (could revoke statute, adjust duration)
 *   - Public Commons / Eventual Readers: Secondary victim (powerless/trapped) — locked out of access after author's monopoly period; loses claim to works that never revert due to duration extension
 *   - Enlightenment Reformers: Analytical observer (analytical/arbitrage) — articulate the sunset intent (temporary incentive, eventual reversion); see constraint as scaffold with genuine exit strategy built in
 *   - Contemporary Legal Interpreter: Meta-observer (analytical/analytical) — recognizes that the category coherence (the thinkability gain) is precisely what enabled subsequent extraction (perpetual extension, scope expansion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.38).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.45).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Thinkability Reading (1710 Statutory Coherence)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '7e7cd076-3e5e-4c78-8c4b-afea73912ead').
narrative_ontology:cs_kernel_codification('7e7cd076-3e5e-4c78-8c4b-afea73912ead', fixed_text).
narrative_ontology:cs_authority_grounding('7e7cd076-3e5e-4c78-8c4b-afea73912ead', extraction).
narrative_ontology:cs_interpretation_layer_present('7e7cd076-3e5e-4c78-8c4b-afea73912ead').
narrative_ontology:cs_reading_relation('7e7cd076-3e5e-4c78-8c4b-afea73912ead', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e7cd076-3e5e-4c78-8c4b-afea73912ead', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('7e7cd076-3e5e-4c78-8c4b-afea73912ead', foundational, copyright_as_conceptual_emergence).
narrative_ontology:cs_axiom_status(copyright_as_conceptual_emergence, holdable).
narrative_ontology:cs_axiom_grounding('7e7cd076-3e5e-4c78-8c4b-afea73912ead', copyright_as_conceptual_emergence, conventional).
narrative_ontology:cs_axiom('7e7cd076-3e5e-4c78-8c4b-afea73912ead', foundational, thinkability_enables_enforcement).
narrative_ontology:cs_axiom_status(thinkability_enables_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('7e7cd076-3e5e-4c78-8c4b-afea73912ead', thinkability_enables_enforcement, instrumental).
narrative_ontology:cs_reference_frame('7e7cd076-3e5e-4c78-8c4b-afea73912ead', guild_privilege_distributed_regime).
narrative_ontology:cs_drift_state('7e7cd076-3e5e-4c78-8c4b-afea73912ead', post_statute_anne_1710, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('7e7cd076-3e5e-4c78-8c4b-afea73912ead', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, stationers_guild).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, book_proprietors).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, public_commons_circulation).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, printers_outside_privilege).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNAUTHORIZED PRINTER — Trapped in a world where 'copy right' has no legal vocabulary. Cannot exit via legitimate claim-making because the category does not exist. Full extraction: suppressed ability to print English-language books without license from Stationers Company. No alternative legal framework available within biographical horizon. Pure snare from this structural position.
constraint_indexing:constraint_classification(ip_category_emergence__thinkability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATIONER-PROPRIETOR — After 1710, experiences mixed coordination and extraction. Statute provides genuine coordination: standardizes duration (14+14 years), establishes registration, enables market transfer of rights. But statute also entrenches extractive control: transforms perpetual common-law privilege into statutory monopoly; centralizes licensing authority. Constrained exit: proprietor benefits from property rights framework but cannot exit the statutory regime without forfeiting the entire rights apparatus. Moderate power with constrained options produces tangled rope.
constraint_indexing:constraint_classification(ip_category_emergence__thinkability_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENT/CROWN AUTHORITY — Sees 1710 statutory coherence as solving a coordination problem: stabilizing the monopoly regime that generated revenue and political control. From the Crown's perspective, creating a unified legal category ('copy right' as distinct from guild privilege) enables regulatory arbitrage — switching between regulatory forms (royal grant, common-law, statutory) without internal contradiction. Parliament gains power by converting a distributed guild privilege into a state-administered category. Low experienced extraction because the authority has full exit options and net benefit from regime establishment.
constraint_indexing:constraint_classification(ip_category_emergence__thinkability_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ENLIGHTENMENT REFORM INTENT — The normative project embedded in the Statute of Anne (1710) explicitly declares a sunset logic: the preamble frames IP protection as temporary incentive ('for the encouragement of learning') with eventual reversion to public commons. The stated intent is that authors/proprietors receive limited monopoly, after which works enter common ownership. This perspective sees the constraint as temporary scaffolding — enabling book production at scale while holding in reserve the public's ultimate claim to knowledge. However, the actual trajectory (perpetual extension, expansion of scope) contradicts the sunset intent. The scaffold classification reflects the **stated architecture**, not the actual outcome.
constraint_indexing:constraint_classification(ip_category_emergence__thinkability_reading, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEGRADED CATEGORY APPARATUS — From a civilizational view, 1710 created a statutory category ('copy right') whose foundational coherence has eroded. The statute explicitly defined limited duration and public reversion. Contemporary copyright law has inverted these: perpetual extension (life+70), expanded scope (mechanical, digital, derivative), and suppressed public access. The category persists through institutional inertia — contractual entrenchment, lobbying power, path dependency — not because the original justification holds. Theater ratio (0.62) reflects that contemporary IP protection is substantially performative: it claims to incentivize creation while blocking access to foundational texts, creating the ritualistic appearance of legitimate monopoly without the coordinating function.
constraint_indexing:constraint_classification(ip_category_emergence__thinkability_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW — The strong temptation is to see IP category emergence as an inevitable, unchangeable natural law: ideas require protection frameworks; creators need incentives; property rights are inherent. This perspective naturalizes the 1710 moment as the 'discovery' of an always-existing category (copy right as natural right in authored expression). However, the structural data contradicts this: the category is thoroughly constructed; it emerges from specific guild privilege dynamics, Crown revenue interests, and Enlightenment discourse; it is contingent on statutory framing. The mountain classification fails the false summit test: beneficiaries exist (stationers, proprietors), victims exist (unauthorized printers, public commons), and active enforcement is required. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(ip_category_emergence__thinkability_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ip_category_emergence__thinkability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ip_category_emergence__thinkability_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, TR),
    TR >= 0.70.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The statute creates a coordinated mechanism (duration rules, registration, transferability) alongside extractive control (enforced monopoly, suppressed alternatives). The extractiveness is lower than it would be without coordination because the statute genuinely standardizes and limits the regime—unauthorized printers are suppressed, but under knowable rules. If the constraint were pure guild privilege (pre-1710 framing), extractiveness would be lower (no competitors, but also no registration or transfer mechanism). If the constraint were contemporary IP (perpetual extension, no actual reversion), extractiveness would be higher. The 0.38 reflects that 1710 statute balances coordination and extraction. Theater ratio (0.62): Moderate-high. The statute's theater emerges from the gap between the preamble's stated intent (temporary incentive) and the actual enforcement (perpetual entrenchment). Proprietors must perform the role of incentivized creators defending public interest; Parliament must perform the role of temporary grant-giver rather than perpetual landlord. By 1750, the theater has visibly increased as duration extensions begin and the 'eventual reversion' becomes increasingly fictional. Suppression (0.45): Moderate. Pre-1710, suppression is distributed and implicit (guild enforcement, Crown grants, common-law precedent applied ad hoc). Post-1710, suppression becomes standardized and explicit (statute defines who may print, how registration works, what penalties apply). The quantitative level of suppression (how many printers are locked out, how completely) may not change, but the structure changes from implicit to codified. Moderate suppression reflects that the statute creates enforceable rules but also allows some exits (authors can publish directly to proprietor, proprietor can transfer rights, duration eventually expires).
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap is between those who experience the category as coordination (Parliament, proprietors with transfer options) and those who experience it as pure suppression (unauthorized printers, locked-out public). The statute frames itself as coordination (temporary incentive, eventual reversion) but produces experiences ranging from rope (beneficiary view) to piton (contemporary view as theater increases) to snare (powerless victim view). The thinkability reading reveals that the category emergence is the mechanism that *allows* this gap to exist: once 'copy right' is coherent, disputes are reframed into property-law terms, and alternative vocabulary (guild privilege, commons claim, natural right of readers) becomes strategically inaccessible.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural relationship to the category emergence. The proprietor (beneficiary + arbitrage) experiences low d: they benefit from the new category and can exit by not registering, or by selling rights, or by deploying works into alternative formats. The unauthorized printer (victim + trapped in pre-1710 frame) experiences high d: cannot exit because the category has eliminated the vocabulary for alternative claims. The public commons agent (victim + trapped long-term) experiences extremely high d: the statute frames eventual reversion as coordination, but subsequent extensions eliminate the exit (reversion never occurs). Parliament (beneficiary + arbitrage, institutional) experiences very low d: can adjust the statute at will, has no dependence on the regime, net benefit from regulatory authority. Directional flows concentrate extraction onto those with no vocabulary for contesting the regime—and the category emergence ensures that such vocabulary becomes structurally unavailable within the legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is a **reading** of a contested kernel, not a unified phenomenon. The statute of 1710 simultaneously coordinates (unifies dispersed rules into coherent category) and extracts (standardizes suppression, enables perpetual entrenchment). The thinkability reading emphasizes that the category emergence is itself the critical structural feature: the statute doesn't just enforce property rights—it creates the conceptual apparatus in which such rights become legible and challengeable only within the property frame. This makes the constraint tangled rope from the moderately-powered victim's perspective (can partially exit by navigating the property regime) and snare from the powerless victim's perspective (cannot exit because the vocabulary for contesting is eliminated). The false summit (natural law view) is a tempting error: the temptation is to see IP rights as inherent in authored expression, grounded in labor or personality. The structural data contradicts this: the category is constructed, the beneficiaries are identifiable, enforcement is active and increasing. The mandate-trophy is resolved by accepting that the constraint is a **created category that produces incommensurable experiences** depending on structural position—not a natural law, not pure coordination, not pure extraction, but a tangled hybrid that can only be analyzed from a specific perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_thinkability_vs_discovery,
    'Did the 1710 Statute of Anne DISCOVER a pre-existing category (copy right as inherent in authored expression) or CREATE a new legal-epistemic category through statutory coherence?',
    'Analysis of pre-1710 guild privilege doctrine and common-law case law: did jurists deploy vocabulary and conceptual frameworks equivalent to ''copy right'' before 1710? Or was the statute itself the first systematic articulation?',
    'If discovery: the constraint approaches mountain (natural law), and beneficiary presence is merely alignment with nature. If creation: the constraint is tangled_rope (constructed coordination+extraction), and beneficiary presence reveals institutional construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_thinkability_vs_discovery, conceptual, 'Whether 1710 discovered or created the IP category').

omega_variable(
    guild_privilege_vs_individual_property,
    'Is statutory IP (1710+) a transformation of collective guild privilege into individual property, or a wholly new regime that merely displaced the guild?',
    'Doctrinal genealogy: tracing whether statutory ''copy right'' incorporates guild privilege logic (collective monopoly over production) or reverses it (individual ownership of intangible asset). Examine statutory language on duration, transfer, and reversion.',
    'If transformation: the constraint exhibits institutional continuity (piton classification more apt — degraded guild ritual). If new regime: the constraint is a structural break with distinct ε and beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guild_privilege_vs_individual_property, conceptual, 'Genealogy of guild privilege to individual IP property').

omega_variable(
    sunset_clause_intentionality,
    'Was the Statute of Anne''s temporary duration (14+14 years, then reversion to public commons) a genuine structural intention or rhetorical cover for perpetual entrenchment?',
    'Historical analysis of parliamentary debate, preamble language, and early-18th-century enforcement: did Crown/Parliament intend and expect works to actually revert to commons? Or was the sunset a fig leaf for perpetual monopoly?',
    'If genuine: scaffold classification is structurally justified; the constraint contains its own sunset mechanism. If rhetorical cover: the scaffold is aspirational, not actual; the constraint is actually tangled_rope with concealed extractive intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_intentionality, empirical, 'Whether 1710 sunset clause reflected genuine structural intention').

omega_variable(
    thinkability_enables_extraction,
    'Does the emergence of ''copy right'' as a coherent legal category ENABLE extraction that was previously impossible (by creating enforceable property claims), or does it REDUCE extraction (by substituting defined statutory limits for unlimited guild suppression)?',
    'Comparative analysis: measuring suppression and extractiveness under pre-1710 guild privilege vs post-1710 statutory regime. Did the category emergence concentrate or distribute the extraction mechanism?',
    'If enables extraction: the thinkability reading is correct — the category emergence is the extraction mechanism, and statute provides the coherence for unprecedented control. If reduces extraction: the statute genuinely coordinated a prior mess, and thinkability is secondary to coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_enables_extraction, empirical, 'Whether category emergence enabled or reduced extraction').

omega_variable(
    reading_identity_lock_on_category,
    'Does this reading lock its own interpretive position into the thinkability frame (conceptual emergence as the binding mechanism) such that alternative readings (first_holding, synchronic_diachronic) become categorically unavailable?',
    'Meta-analysis of the reading''s axiom structure: if axiom_copyright_as_conceptual_emergence is foundational and holdable, can first_holding_reading maintain its core axiom that the ''right'' was always present in authorship (discovery not creation)? Or does thinkability foreclose discovery?',
    'If forecloses: coexists_with relation is wrong; relation should be ''forecloses''. If coexists: the reading has correctly assessed that thinkability and discovery are orthogonal (one reads category as newly coherent; the other reads content as pre-existing but newly codified).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_lock_on_category, conceptual, 'Whether thinkability reading forecloses discovery reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipcat_theater_1700, ip_category_emergence__thinkability_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ipcat_theater_1710, ip_category_emergence__thinkability_reading, theater_ratio, 1, 0.52).
narrative_ontology:measurement(ipcat_theater_1750, ip_category_emergence__thinkability_reading, theater_ratio, 2, 0.62).

% Extraction over time
narrative_ontology:measurement(ipcat_extract_1700, ip_category_emergence__thinkability_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ipcat_extract_1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1, 0.35).
narrative_ontology:measurement(ipcat_extract_1750, ip_category_emergence__thinkability_reading, base_extractiveness, 2, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, perpetual_copyright_extension).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, public_domain_reversion_failure).

% DUAL FORMULATION NOTE:
% The IP category emergence is formulated as three distinct constraint stories in the ip_category_emergence kernel family. This story (thinkability_reading) emphasizes category coherence as binding mechanism. first_holding_reading emphasizes pre-existing authorial right made enforceable. synchronic_diachronic_seam emphasizes structural rupture. All three share the 1710 Statute as reference point but analyze different structural features. The thinkability reading is upstream to perpetual_copyright_extension (extension is only possible because the category is now coherent and tradeable) and downstream from public_domain_reversion_failure (eventual failure of reversion was enabled by the category's coherence, which allowed the duration mechanism to be extended indefinitely).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
